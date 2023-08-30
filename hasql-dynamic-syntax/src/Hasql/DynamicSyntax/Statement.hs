{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Hasql.DynamicSyntax.Statement (
  StatementBuilder,
  HasTargetResultHandler (..),
  buildStatement,
  renderStatement,
  encodeParam,
  param,
) where

import Control.Monad.Trans.RWS (RWS, runRWS, rws)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant ((>$))
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicSyntax.Ast
import qualified Hasql.DynamicSyntax.Ast.Internal as Internal
import Hasql.Encoders (Params)
import qualified Hasql.Encoders as Encoders
import Hasql.Implicits.Encoders (DefaultParamEncoder (defaultParam))
import Hasql.Statement (Statement (..))
import qualified PostgresqlSyntax.Ast as P
import qualified PostgresqlSyntax.Rendering as Render

-- | Allocate a parameter value with an explicit encoder.
encodeParam :: Encoders.NullableOrNot Encoders.Value param -> param -> StatementBuilder Param
encodeParam encoder p = StatementBuilder $ rws \_ paramCount ->
  (Param $ Internal.Param $ paramCount + 1, paramCount + 1, p >$ Encoders.param encoder)

-- | Allocate a parameter value with an implicit encoder.
param :: (DefaultParamEncoder param) => param -> StatementBuilder Param
param = encodeParam defaultParam

-- | Build a hasql @Statement@ from a @StatementBuilder@
buildStatement
  :: (HasTargetListResultHandler rows r)
  => TargetListResultHandler rows r
  -- ^ A function to map the result row fields into a data structure.
  -> (Decoders.Row r -> Decoders.Result a)
  -- ^ How to decode all results given a decoder for a single result (e.g. a single row, a Maybe row, a list of rows, etc...)
  -> StatementBuilder (PreparableStmt rows)
  -- ^ The statement builder expression to build.
  -> Statement () a
buildStatement handler toResult stmt =
  Statement sql params (toResult $ handleTargetResultList handler <$> compileListDecoders rows) (paramCount > 0)
  where
    ((stmt', rows), paramCount, params) = runRWS (runStatementBuilder $ buildPreparableStmt <$> stmt) () 0
    sql = Render.toByteString $ Render.preparableStmt stmt'

-- | Useful for debugging. Print the preparable SQL statement for a statement builder expression.
renderStatement :: StatementBuilder (PreparableStmt row) -> Text
renderStatement stmt = sql
  where
    ((stmt', _), _, _) = runRWS (runStatementBuilder $ buildPreparableStmt <$> stmt) () 0
    sql = Render.toText $ Render.preparableStmt stmt'

-- | A monadic context for allocating query parameters and building preparable statements.
newtype StatementBuilder a = StatementBuilder {runStatementBuilder :: RWS () (Params ()) Int a}
  deriving newtype (Functor, Applicative, Monad)

-- | A typeclass with an associated type family for computing the handler function for a set of query target result rows.
class HasTargetListResultHandler rows a where
  -- | The type of handler for this list of result rows.
  type TargetListResultHandler rows a :: Type

  -- | Execute the handler by applying a heterogeneous list of result rows to it.
  handleTargetResultList :: TargetListResultHandler rows a -> TargetListResult rows -> a

-- | Handle an empty list of rows by returning the result.
instance HasTargetListResultHandler '[] a where
  type TargetListResultHandler '[] a = a
  handleTargetResultList a _ = a

-- | Handle a cons-cell of result rows by applying the head result row, then running the handler for the result of the
-- result rows.
instance
  ( HasTargetResultHandler row (TargetListResultHandler rows a)
  , HasTargetListResultHandler rows a
  )
  => HasTargetListResultHandler (row ': rows) a
  where
  type TargetListResultHandler (row ': rows) a = TargetResultHandler row (TargetListResultHandler rows a)
  handleTargetResultList handler (ListResultCons row rows) =
    handleTargetResultList (handleTargetResult handler row) rows

-- | A typeclass with an associated type family for computing the handler function for a single query target result row.
class HasTargetResultHandler row a where
  -- | The type of handler for this result row.
  type TargetResultHandler row a :: Type

  -- | Execute the handler by applying a heterogeneous list to it.
  handleTargetResult :: TargetResultHandler row a -> TargetResult row -> a

-- | Handle an row by returning the result.
instance HasTargetResultHandler '[] a where
  type TargetResultHandler '[] a = a
  handleTargetResult a _ = a

-- | Handle a cons-cell row by applying the head row field, then running the handler for the result of the row.
instance (HasTargetResultHandler row a) => HasTargetResultHandler (t ': row) a where
  type TargetResultHandler (t ': row) a = ColumnToHask t -> TargetResultHandler row a
  handleTargetResult f (ResultCons t row) = handleTargetResult (f t) row

-- * Internal

buildPreparableStmt :: PreparableStmt row -> (P.PreparableStmt, TargetListDecoders row)
buildPreparableStmt = \case
  SelectPreparableStmt stmt -> first P.SelectPreparableStmt $ buildSelectStmt stmt
  InsertPreparableStmt stmt -> first P.InsertPreparableStmt $ buildInsertStmt stmt
  UpdatePreparableStmt stmt -> first P.UpdatePreparableStmt $ buildUpdateStmt stmt
  DeletePreparableStmt stmt -> first P.DeletePreparableStmt $ buildDeleteStmt stmt
  CallPreparableStmt stmt -> first P.CallPreparableStmt $ buildCallStmt stmt

buildSelectStmt :: SelectStmt row -> (P.SelectStmt, TargetListDecoders row)
buildSelectStmt = \case
  Left s -> first Left $ buildSelectNoParens s
  Right s -> first Right $ buildSelectWithParens s

buildInsertStmt :: InsertStmt row -> (P.InsertStmt, TargetListDecoders row)
buildInsertStmt = \case
  InsertStmt with target rest onConflict returning -> do
    let with' = buildWithClause <$> with
    let target' = buildInsertTarget target
    let rest' = buildInsertRest rest
    let onConflict' = buildOnConflict <$> onConflict
    let (returning', row) = buildReturningClause returning
    (P.InsertStmt with' target' rest' onConflict' returning', row)

buildUpdateStmt :: UpdateStmt row -> (P.UpdateStmt, TargetListDecoders row)
buildUpdateStmt = \case
  UpdateStmt with rel setClauseList from whereOrCurrent returning -> do
    let with' = buildWithClause <$> with
    let rel' = buildRelationExprOptAlias rel
    let setClauseList' = buildSetClauseList setClauseList
    let from' = buildFromClause <$> from
    let whereOrCurrent' = buildWhereOrCurrentClause <$> whereOrCurrent
    let (returning', row) = buildReturningClause returning
    (P.UpdateStmt with' rel' setClauseList' from' whereOrCurrent' returning', row)

buildDeleteStmt :: DeleteStmt row -> (P.DeleteStmt, TargetListDecoders row)
buildDeleteStmt = \case
  DeleteStmt with rel usingClause whereOrCurrent returning -> do
    let with' = buildWithClause <$> with
    let rel' = buildRelationExprOptAlias rel
    let usingClause' = buildUsingClause <$> usingClause
    let whereOrCurrent' = buildWhereOrCurrentClause <$> whereOrCurrent
    let (returning', row) = buildReturningClause returning
    (P.DeleteStmt with' rel' usingClause' whereOrCurrent' returning', row)

buildCallStmt :: CallStmt row -> (P.CallStmt, TargetListDecoders row)
buildCallStmt = \case
  CallStmt row funcApplication -> (P.CallStmt $ buildFuncApplication funcApplication, targetListDecoders row)

buildSelectWithParens :: SelectWithParens row -> (P.SelectWithParens, TargetListDecoders row)
buildSelectWithParens = \case
  NoParensSelectWithParens s -> first P.NoParensSelectWithParens $ buildSelectNoParens s
  WithParensSelectWithParens s -> first P.WithParensSelectWithParens $ buildSelectWithParens s

buildSelectNoParens :: SelectNoParens row -> (P.SelectNoParens, TargetListDecoders row)
buildSelectNoParens = \case
  SelectNoParens with select sort limit forLocking -> do
    let with' = buildWithClause <$> with
    let (select', row) = buildSelectClause select
    let sort' = buildSortClause <$> sort
    let limit' = buildSelectLimit <$> limit
    let forLocking' = buildForLockingClause <$> forLocking
    (P.SelectNoParens with' select' sort' limit' forLocking', row)

buildWithClause :: WithClause -> P.WithClause
buildWithClause = \case
  WithClause rec ctes -> P.WithClause rec $ buildCommonTableExpr <$> ctes

buildCommonTableExpr :: CommonTableExpr -> P.CommonTableExpr
buildCommonTableExpr = \case
  CommonTableExpr name cols materialized stmt ->
    P.CommonTableExpr name cols materialized . fst $ buildPreparableStmt stmt

buildSelectClause :: SelectClause row -> (P.SelectClause, TargetListDecoders row)
buildSelectClause = \case
  Left s -> first Left $ buildSimpleSelect s
  Right s -> first Right $ buildSelectWithParens s

buildSimpleSelect :: SimpleSelect row -> (P.SimpleSelect, TargetListDecoders row)
buildSimpleSelect = \case
  NormalSimpleSelect targeting into from where_ group having window -> do
    let (targeting', row) = buildTargeting targeting
    let into' = buildIntoClause <$> into
    let from' = buildFromClause <$> from
    let where_' = buildAExpr <$> where_
    let group' = buildGroupClause <$> group
    let having' = buildAExpr <$> having
    let window' = buildWindowClause <$> window
    (P.NormalSimpleSelect targeting' into' from' where_' group' having' window', row)
  ValuesSimpleSelect row values ->
    (,targetListDecoders row) . P.ValuesSimpleSelect $ buildValuesClause values
  TableSimpleSelect row rel ->
    (,targetListDecoders row) . P.TableSimpleSelect $ buildRelationExpr rel
  BinSimpleSelect op lhs allOrDistinct rhs -> do
    let (lhs', row) = buildSelectClause lhs
    let (rhs', _) = buildSelectClause rhs
    (P.BinSimpleSelect op lhs' allOrDistinct rhs', row)

buildRelationExpr :: RelationExpr -> P.RelationExpr
buildRelationExpr = \case
  SimpleRelationExpr name asterisk -> P.SimpleRelationExpr (buildQualifiedName name) asterisk
  OnlyRelationExpr name parens -> P.OnlyRelationExpr (buildQualifiedName name) parens

buildQualifiedName :: QualifiedName -> P.QualifiedName
buildQualifiedName = \case
  SimpleQualifiedName ident -> P.SimpleQualifiedName ident
  IndirectedQualifiedName ident indirection -> P.IndirectedQualifiedName ident $ buildIndirection indirection

buildIndirection :: Indirection -> P.Indirection
buildIndirection = fmap buildIndirectionEl

buildIndirectionEl :: IndirectionEl -> P.IndirectionEl
buildIndirectionEl = \case
  AttrNameIndirectionEl ident -> P.AttrNameIndirectionEl ident
  AllIndirectionEl -> P.AllIndirectionEl
  ExprIndirectionEl expr -> P.ExprIndirectionEl $ buildAExpr expr
  SliceIndirectionEl lowBound hiBound -> P.SliceIndirectionEl (buildAExpr <$> lowBound) $ buildAExpr <$> hiBound

buildAExpr :: (IsAExpr a) => a -> P.AExpr
buildAExpr a = case toAExpr a of
  CExprAExpr expr -> P.CExprAExpr $ buildCExpr expr
  TypecastAExpr expr ty -> P.TypecastAExpr (buildAExpr expr) (buildTypename ty)
  CollateAExpr expr collation -> P.CollateAExpr (buildAExpr expr) collation
  AtTimeZoneAExpr lhs rhs -> P.AtTimeZoneAExpr (buildAExpr lhs) (buildAExpr rhs)
  PlusAExpr expr -> P.PlusAExpr $ buildAExpr expr
  MinusAExpr expr -> P.MinusAExpr $ buildAExpr expr
  SymbolicBinOpAExpr lhs op rhs -> P.SymbolicBinOpAExpr (buildAExpr lhs) op (buildAExpr rhs)
  PrefixQualOpAExpr op expr -> P.PrefixQualOpAExpr op $ buildAExpr expr
  SuffixQualOpAExpr expr op -> flip P.SuffixQualOpAExpr op $ buildAExpr expr
  AndAExpr lhs rhs -> P.AndAExpr (buildAExpr lhs) (buildAExpr rhs)
  OrAExpr lhs rhs -> P.OrAExpr (buildAExpr lhs) (buildAExpr rhs)
  NotAExpr expr -> P.NotAExpr $ buildAExpr expr
  VerbalExprBinOpAExpr lhs not_ op rhs escape ->
    P.VerbalExprBinOpAExpr
      (buildAExpr lhs)
      not_
      op
      (buildAExpr rhs)
      (buildAExpr <$> escape)
  ReversableOpAExpr expr not_ op ->
    P.ReversableOpAExpr (buildAExpr expr) not_ (buildAExprReversableOp op)
  IsnullAExpr expr -> P.IsnullAExpr $ buildAExpr expr
  NotnullAExpr expr -> P.NotnullAExpr $ buildAExpr expr
  OverlapsAExpr lhs rhs -> P.OverlapsAExpr (buildRow lhs) (buildRow rhs)
  SubqueryAExpr expr op subtype sub ->
    P.SubqueryAExpr
      (buildAExpr expr)
      op
      subtype
      (bimap (fst . buildSelectWithParens) buildAExpr sub)
  UniqueAExpr select -> P.UniqueAExpr . fst $ buildSelectWithParens select
  DefaultAExpr -> P.DefaultAExpr

buildCExpr :: CExpr -> P.CExpr
buildCExpr = \case
  ColumnrefCExpr col -> P.ColumnrefCExpr $ buildColumnref col
  AexprConstCExpr expr -> P.AexprConstCExpr $ buildAExprConst expr
  ParamCExpr (Param (Internal.Param i)) -> P.ParamCExpr i Nothing
  InParensCExpr expr indirection -> P.InParensCExpr (buildAExpr expr) (buildIndirection <$> indirection)
  CaseCExpr case_ -> P.CaseCExpr $ buildCaseExpr case_
  FuncCExpr func -> P.FuncCExpr $ buildFuncExpr func
  SelectWithParensCExpr select indirection ->
    P.SelectWithParensCExpr (fst $ buildSelectWithParens select) (buildIndirection <$> indirection)
  ExistsCExpr select -> P.ExistsCExpr . fst $ buildSelectWithParens select
  ArrayCExpr array -> P.ArrayCExpr $ bimap (fst . buildSelectWithParens) buildArrayExpr array
  ExplicitRowCExpr row -> P.ExplicitRowCExpr $ buildExplicitRow row
  ImplicitRowCExpr row -> P.ImplicitRowCExpr $ buildImplicitRow row
  GroupingCExpr exprs -> P.GroupingCExpr $ buildExprList exprs

buildColumnref :: Columnref -> P.Columnref
buildColumnref = \case
  Columnref col indirection -> P.Columnref col $ buildIndirection <$> indirection

buildAExprConst :: AexprConst -> P.AexprConst
buildAExprConst = \case
  IAexprConst i -> P.IAexprConst i
  FAexprConst f -> P.FAexprConst f
  SAexprConst s -> P.SAexprConst s
  BAexprConst b -> P.BAexprConst b
  XAexprConst x -> P.XAexprConst x
  FuncAexprConst funcName args s ->
    P.FuncAexprConst
      (buildFuncName funcName)
      (buildFuncConstArgs <$> args)
      s
  ConstTypenameAexprConst typename s -> P.ConstTypenameAexprConst (buildConstTypename typename) s
  StringIntervalAexprConst s interval -> P.StringIntervalAexprConst s interval
  IntIntervalAexprConst i s -> P.IntIntervalAexprConst i s
  BoolAexprConst b -> P.BoolAexprConst b
  NullAexprConst -> P.NullAexprConst

buildFuncName :: FuncName -> P.FuncName
buildFuncName = \case
  TypeFuncName name -> P.TypeFuncName name
  IndirectedFuncName col indirection -> P.IndirectedFuncName col $ buildIndirection indirection

buildFuncConstArgs :: FuncConstArgs -> P.FuncConstArgs
buildFuncConstArgs = \case
  FuncConstArgs args sort -> P.FuncConstArgs (buildFuncArgExpr <$> args) (buildSortClause <$> sort)

buildFuncArgExpr :: FuncArgExpr -> P.FuncArgExpr
buildFuncArgExpr = \case
  ExprFuncArgExpr expr -> P.ExprFuncArgExpr $ buildAExpr expr
  ColonEqualsFuncArgExpr ident expr -> P.ColonEqualsFuncArgExpr ident $ buildAExpr expr
  EqualsGreaterFuncArgExpr ident expr -> P.EqualsGreaterFuncArgExpr ident $ buildAExpr expr

buildConstTypename :: ConstTypename -> P.ConstTypename
buildConstTypename = \case
  NumericConstTypename numeric -> P.NumericConstTypename $ buildNumeric numeric
  ConstBitConstTypename constBit -> P.ConstBitConstTypename $ buildConstBit constBit
  ConstCharacterConstTypename constChar -> P.ConstCharacterConstTypename constChar
  ConstDatetimeConstTypename constDatetime -> P.ConstDatetimeConstTypename constDatetime

buildNumeric :: Numeric -> P.Numeric
buildNumeric = \case
  IntNumeric -> P.IntNumeric
  IntegerNumeric -> P.IntegerNumeric
  SmallintNumeric -> P.SmallintNumeric
  BigintNumeric -> P.BigintNumeric
  RealNumeric -> P.RealNumeric
  FloatNumeric float -> P.FloatNumeric float
  DoublePrecisionNumeric -> P.DoublePrecisionNumeric
  DecimalNumeric mods -> P.DecimalNumeric $ buildTypeModifiers <$> mods
  DecNumeric mods -> P.DecNumeric $ buildTypeModifiers <$> mods
  NumericNumeric mods -> P.NumericNumeric $ buildTypeModifiers <$> mods
  BooleanNumeric -> P.BooleanNumeric

buildTypeModifiers :: TypeModifiers -> P.TypeModifiers
buildTypeModifiers = buildExprList

buildConstBit :: ConstBit -> P.ConstBit
buildConstBit = buildBit

buildBit :: ConstBit -> P.ConstBit
buildBit = \case
  Bit varying exprs -> P.Bit varying $ buildExprList <$> exprs

buildCaseExpr :: CaseExpr -> P.CaseExpr
buildCaseExpr = \case
  CaseExpr arg clauseList def ->
    P.CaseExpr
      (buildAExpr <$> arg)
      (buildWhenClauseList clauseList)
      (buildAExpr <$> def)

buildWhenClauseList :: WhenClauseList -> P.WhenClauseList
buildWhenClauseList = fmap buildWhenClause

buildWhenClause :: WhenClause -> P.WhenClause
buildWhenClause = \case
  WhenClause a b -> P.WhenClause (buildAExpr a) (buildAExpr b)

buildFuncExpr :: FuncExpr -> P.FuncExpr
buildFuncExpr = \case
  ApplicationFuncExpr application withinGroup filter_ over ->
    P.ApplicationFuncExpr
      (buildFuncApplication application)
      (buildWithinGroupClause <$> withinGroup)
      (buildAExpr <$> filter_)
      (buildOverClause <$> over)
  SubexprFuncExpr subexpr -> P.SubexprFuncExpr $ buildFuncExprCommonSubexpr subexpr

buildFuncExprCommonSubexpr :: FuncExprCommonSubexpr -> P.FuncExprCommonSubexpr
buildFuncExprCommonSubexpr = \case
  CollationForFuncExprCommonSubexpr expr -> P.CollationForFuncExprCommonSubexpr $ buildAExpr expr
  CurrentDateFuncExprCommonSubexpr -> P.CurrentDateFuncExprCommonSubexpr
  CurrentTimeFuncExprCommonSubexpr i -> P.CurrentTimeFuncExprCommonSubexpr i
  CurrentTimestampFuncExprCommonSubexpr i -> P.CurrentTimestampFuncExprCommonSubexpr i
  LocalTimeFuncExprCommonSubexpr i -> P.LocalTimeFuncExprCommonSubexpr i
  LocalTimestampFuncExprCommonSubexpr i -> P.LocalTimestampFuncExprCommonSubexpr i
  CurrentRoleFuncExprCommonSubexpr -> P.CurrentRoleFuncExprCommonSubexpr
  CurrentUserFuncExprCommonSubexpr -> P.CurrentUserFuncExprCommonSubexpr
  SessionUserFuncExprCommonSubexpr -> P.SessionUserFuncExprCommonSubexpr
  UserFuncExprCommonSubexpr -> P.UserFuncExprCommonSubexpr
  CurrentCatalogFuncExprCommonSubexpr -> P.CurrentCatalogFuncExprCommonSubexpr
  CurrentSchemaFuncExprCommonSubexpr -> P.CurrentSchemaFuncExprCommonSubexpr
  CastFuncExprCommonSubexpr expr typename ->
    P.CastFuncExprCommonSubexpr
      (buildAExpr expr)
      (buildTypename typename)
  ExtractFuncExprCommonSubexpr extracts -> P.ExtractFuncExprCommonSubexpr $ buildExtractList <$> extracts
  OverlayFuncExprCommonSubexpr overlays -> P.OverlayFuncExprCommonSubexpr $ buildOverlayList overlays
  PositionFuncExprCommonSubexpr positions -> P.PositionFuncExprCommonSubexpr $ buildPositionList <$> positions
  SubstringFuncExprCommonSubexpr substrs -> P.SubstringFuncExprCommonSubexpr $ buildSubstrList <$> substrs
  TreatFuncExprCommonSubexpr expr typename ->
    P.TreatFuncExprCommonSubexpr
      (buildAExpr expr)
      (buildTypename typename)
  TrimFuncExprCommonSubexpr trimMod trimList -> P.TrimFuncExprCommonSubexpr trimMod $ buildTrimList trimList
  NullIfFuncExprCommonSubexpr lhs rhs -> P.NullIfFuncExprCommonSubexpr (buildAExpr lhs) (buildAExpr rhs)
  CoalesceFuncExprCommonSubexpr exprs -> P.CoalesceFuncExprCommonSubexpr $ buildExprList exprs
  GreatestFuncExprCommonSubexpr exprs -> P.GreatestFuncExprCommonSubexpr $ buildExprList exprs
  LeastFuncExprCommonSubexpr exprs -> P.LeastFuncExprCommonSubexpr $ buildExprList exprs

buildExtractList :: ExtractList -> P.ExtractList
buildExtractList = \case
  ExtractList arg expr -> P.ExtractList arg $ buildAExpr expr

buildOverlayList :: OverlayList -> P.OverlayList
buildOverlayList = \case
  OverlayList expr placing substrFrom substrFor ->
    P.OverlayList
      (buildAExpr expr)
      (buildAExpr placing)
      (buildAExpr substrFrom)
      (buildAExpr <$> substrFor)

buildPositionList :: PositionList -> P.PositionList
buildPositionList = \case
  PositionList lhs rhs -> P.PositionList (buildBExpr lhs) (buildBExpr rhs)

buildSubstrList :: SubstrList -> P.SubstrList
buildSubstrList = \case
  ExprSubstrList expr fromFor -> P.ExprSubstrList (buildAExpr expr) (buildSubstrListFromFor fromFor)
  ExprListSubstrList exprs -> P.ExprListSubstrList $ buildExprList exprs

buildSubstrListFromFor :: SubstrListFromFor -> P.SubstrListFromFor
buildSubstrListFromFor = \case
  FromForSubstrListFromFor from for -> P.FromForSubstrListFromFor (buildAExpr from) (buildAExpr for)
  ForFromSubstrListFromFor for from -> P.ForFromSubstrListFromFor (buildAExpr for) (buildAExpr from)
  FromSubstrListFromFor from -> P.FromSubstrListFromFor $ buildAExpr from
  ForSubstrListFromFor for -> P.ForSubstrListFromFor $ buildAExpr for

buildTrimList :: TrimList -> P.TrimList
buildTrimList = \case
  ExprFromExprListTrimList expr exprs -> P.ExprFromExprListTrimList (buildAExpr expr) (buildExprList exprs)
  FromExprListTrimList exprs -> P.FromExprListTrimList $ buildExprList exprs
  ExprListTrimList exprs -> P.ExprListTrimList $ buildExprList exprs

buildFuncApplication :: FuncApplication -> P.FuncApplication
buildFuncApplication = \case
  FuncApplication name params ->
    P.FuncApplication (buildFuncName name) (buildFuncApplicationParams <$> params)

buildFuncApplicationParams :: FuncApplicationParams -> P.FuncApplicationParams
buildFuncApplicationParams = \case
  NormalFuncApplicationParams allOrDistinct args sort ->
    P.NormalFuncApplicationParams
      allOrDistinct
      (buildFuncArgExpr <$> args)
      (buildSortClause <$> sort)
  VariadicFuncApplicationParams args arg sort ->
    P.VariadicFuncApplicationParams
      (fmap buildFuncArgExpr <$> args)
      (buildFuncArgExpr arg)
      (buildSortClause <$> sort)
  StarFuncApplicationParams -> P.StarFuncApplicationParams

buildWithinGroupClause :: WithinGroupClause -> P.WithinGroupClause
buildWithinGroupClause = buildSortClause

buildOverClause :: OverClause -> P.OverClause
buildOverClause = \case
  WindowOverClause window -> P.WindowOverClause $ buildWindowSpecification window
  ColIdOverClause col -> P.ColIdOverClause col

buildWindowSpecification :: WindowSpecification -> P.WindowSpecification
buildWindowSpecification = \case
  WindowSpecification name partition sort frame ->
    P.WindowSpecification
      name
      (buildExprList <$> partition)
      (buildSortClause <$> sort)
      (buildFrameClause <$> frame)

buildFrameClause :: FrameClause -> P.FrameClause
buildFrameClause = \case
  FrameClause mode extent exclusion -> P.FrameClause mode (buildFrameExtent extent) exclusion

buildFrameExtent :: FrameExtent -> P.FrameExtent
buildFrameExtent = \case
  SingularFrameExtent b -> P.SingularFrameExtent $ buildFrameBound b
  BetweenFrameExtent b b' -> P.BetweenFrameExtent (buildFrameBound b) (buildFrameBound b')

buildFrameBound :: FrameBound -> P.FrameBound
buildFrameBound = \case
  UnboundedPrecedingFrameBound -> P.UnboundedFollowingFrameBound
  UnboundedFollowingFrameBound -> P.UnboundedFollowingFrameBound
  CurrentRowFrameBound -> P.CurrentRowFrameBound
  PrecedingFrameBound expr -> P.PrecedingFrameBound $ buildAExpr expr
  FollowingFrameBound expr -> P.FollowingFrameBound $ buildAExpr expr

buildArrayExpr :: ArrayExpr -> P.ArrayExpr
buildArrayExpr = \case
  ExprListArrayExpr exprs -> P.ExprListArrayExpr $ buildExprList exprs
  ArrayExprListArrayExpr exprs -> P.ArrayExprListArrayExpr $ buildArrayExprList exprs
  EmptyArrayExpr -> P.EmptyArrayExpr

buildArrayExprList :: ArrayExprList -> P.ArrayExprList
buildArrayExprList = fmap buildArrayExpr

buildExplicitRow :: ExplicitRow -> P.ExplicitRow
buildExplicitRow = fmap buildExprList

buildImplicitRow :: ImplicitRow -> P.ImplicitRow
buildImplicitRow = \case
  ImplicitRow exprs expr -> P.ImplicitRow (buildExprList exprs) (buildAExpr expr)

buildExprList :: ExprList -> P.ExprList
buildExprList = fmap buildAExpr

buildTypename :: Typename -> P.Typename
buildTypename = \case
  Typename setof name questionMark arrayBounds ->
    P.Typename setof (buildSimpleTypename name) questionMark arrayBounds

buildSimpleTypename :: SimpleTypename -> P.SimpleTypename
buildSimpleTypename = \case
  GenericTypeSimpleTypename ty -> P.GenericTypeSimpleTypename $ buildGenericType ty
  NumericSimpleTypename n -> P.NumericSimpleTypename $ buildNumeric n
  BitSimpleTypename b -> P.BitSimpleTypename $ buildBit b
  CharacterSimpleTypename ch -> P.CharacterSimpleTypename ch
  ConstDatetimeSimpleTypename dt -> P.ConstDatetimeSimpleTypename dt
  ConstIntervalSimpleTypename iv -> P.ConstIntervalSimpleTypename iv

buildGenericType :: GenericType -> P.GenericType
buildGenericType = \case
  GenericType name attrs mods ->
    P.GenericType name attrs $ fmap buildTypeModifiers mods

buildAExprReversableOp :: AExprReversableOp -> P.AExprReversableOp
buildAExprReversableOp = \case
  NullAExprReversableOp -> P.NullAExprReversableOp
  TrueAExprReversableOp -> P.TrueAExprReversableOp
  FalseAExprReversableOp -> P.FalseAExprReversableOp
  UnknownAExprReversableOp -> P.UnknownAExprReversableOp
  DistinctFromAExprReversableOp expr -> P.DistinctFromAExprReversableOp $ buildAExpr expr
  OfAExprReversableOp typeList -> P.OfAExprReversableOp $ buildTypeList typeList
  BetweenAExprReversableOp asymmetric lhs rhs ->
    P.BetweenAExprReversableOp asymmetric (buildBExpr lhs) (buildAExpr rhs)
  BetweenSymmetricAExprReversableOp lhs rhs ->
    P.BetweenSymmetricAExprReversableOp (buildBExpr lhs) (buildAExpr rhs)
  InAExprReversableOp expr -> P.InAExprReversableOp $ buildInExpr expr
  DocumentAExprReversableOp -> P.DocumentAExprReversableOp

buildInExpr :: InExpr -> P.InExpr
buildInExpr = \case
  SelectInExpr select -> P.SelectInExpr . fst $ buildSelectWithParens select
  ExprListInExpr exprs -> P.ExprListInExpr $ buildExprList exprs

buildBExpr :: (IsBExpr a) => a -> P.BExpr
buildBExpr a = case toBExpr a of
  CExprBExpr expr -> P.CExprBExpr $ buildCExpr expr
  TypecastBExpr expr typename -> P.TypecastBExpr (buildBExpr expr) (buildTypename typename)
  PlusBExpr expr -> P.PlusBExpr $ buildBExpr expr
  MinusBExpr expr -> P.MinusBExpr $ buildBExpr expr
  SymbolicBinOpBExpr lhs op rhs -> P.SymbolicBinOpBExpr (buildBExpr lhs) op (buildBExpr rhs)
  QualOpBExpr op expr -> P.QualOpBExpr op $ buildBExpr expr
  IsOpBExpr expr not_ op -> P.IsOpBExpr (buildBExpr expr) not_ (buildBExprIsOp op)

buildBExprIsOp :: BExprIsOp -> P.BExprIsOp
buildBExprIsOp = \case
  DistinctFromBExprIsOp expr -> P.DistinctFromBExprIsOp $ buildBExpr expr
  OfBExprIsOp typeList -> P.OfBExprIsOp $ buildTypeList typeList
  DocumentBExprIsOp -> P.DocumentBExprIsOp

buildTypeList :: TypeList -> P.TypeList
buildTypeList = fmap buildTypename

buildRow :: Row -> P.Row
buildRow = \case
  ExplicitRowRow r -> P.ExplicitRowRow $ buildExplicitRow r
  ImplicitRowRow r -> P.ImplicitRowRow $ buildImplicitRow r

buildValuesClause :: ValuesClause -> P.ValuesClause
buildValuesClause = fmap buildExprList

targetListDecoders :: TargetTypesList rows -> TargetListDecoders rows
targetListDecoders = \case
  TargetTypesListNil -> ListDecodersNil
  TargetTypesListCons target targets -> ListDecodersCons (targetDecoders target) $ targetListDecoders targets

targetDecoders :: TargetTypes row -> TargetDecoders row
targetDecoders = \case
  TargetTypesNil -> DecodersNil
  TargetTypesCons sqlType row -> DecodersCons (Decoders.column $ columnDecoder sqlType) $ targetDecoders row

columnDecoder :: ColumnType a -> Decoders.NullableOrNot Decoders.Value (ColumnToHask a)
columnDecoder = \case
  ColumnType t Null -> Decoders.nullable $ sqlDecoder t
  ColumnType t NotNull -> Decoders.nonNullable $ sqlDecoder t

sqlDecoder :: SqlType t -> Decoders.Value (SqlToHask t)
sqlDecoder = \case
  SqlBool -> Decoders.bool
  SqlInt2 -> Decoders.int2
  SqlInt4 -> Decoders.int4
  SqlInt8 -> Decoders.int8
  SqlFloat4 -> Decoders.float4
  SqlFloat8 -> Decoders.float8
  SqlNumeric -> Decoders.numeric
  SqlChar -> Decoders.char
  SqlText -> Decoders.text
  SqlBytea -> Decoders.bytea
  SqlDate -> Decoders.date
  SqlTimestamp -> Decoders.timestamp
  SqlTimestampz -> Decoders.timestamptz
  SqlTime -> Decoders.time
  SqlTimez -> Decoders.timetz
  SqlInterval -> Decoders.interval
  SqlUUID -> Decoders.uuid
  SqlInet -> Decoders.inet
  SqlJson -> Decoders.json
  SqlJsonb -> Decoders.jsonb
  SqlArray t -> Decoders.vectorArray $ columnDecoder t

buildTargeting :: Targeting row -> (Maybe P.Targeting, TargetListDecoders row)
buildTargeting = \case
  EmptyTargeting -> (Nothing, ListDecodersNil)
  NormalTargeting targets -> first (Just . P.NormalTargeting) $ buildTargetList targets
  AllTargeting targets -> first (Just . P.NormalTargeting) $ buildTargetList targets
  DistinctTargeting exprs targets -> do
    let exprs' = buildExprList <$> exprs
    first (Just . P.DistinctTargeting exprs') $ buildTargetList targets

buildTargetList :: TargetList (a ': row) -> (P.TargetList, TargetListDecoders (a ': row))
buildTargetList ((toTargetElRow -> target0@TargetElRow{}) :. targets) = (target0' NE.:| targets', ListDecodersCons row0 rows)
  where
    (target0', row0) = buildTargetElRow target0
    (targets', rows) = go targets

    go :: TargetList row -> ([P.TargetEl], TargetListDecoders row)
    go = \case
      TargetListNil -> ([], ListDecodersNil)
      (toTargetElRow -> target@TargetElRow{}) :. list -> do
        let (target', row) = buildTargetElRow target
        let (list', row') = go list
        (target' : list', ListDecodersCons row row')

buildTargetElRow :: TargetElRow (t ': row) -> (P.TargetEl, TargetDecoders (t ': row))
buildTargetElRow = \case
  TargetElRow row target -> (,targetDecoders row) $ buildTargetEl target

buildTargetEl :: (IsTargetEl a) => a -> P.TargetEl
buildTargetEl a = case toTargetEl a of
  AliasedExprTargetEl expr alias -> P.AliasedExprTargetEl (buildAExpr expr) alias
  ImplicitlyAliasedExprTargetEl expr alias -> P.ImplicitlyAliasedExprTargetEl (buildAExpr expr) alias
  ExprTargetEl expr -> P.ExprTargetEl $ buildAExpr expr
  AsteriskTargetEl -> P.AsteriskTargetEl

buildIntoClause :: IntoClause -> P.IntoClause
buildIntoClause = \case
  TemporaryOptTempTableName showTable name -> P.TemporaryOptTempTableName showTable $ buildQualifiedName name
  TempOptTempTableName showTable name -> P.TempOptTempTableName showTable $ buildQualifiedName name
  LocalTemporaryOptTempTableName showTable name -> P.LocalTemporaryOptTempTableName showTable $ buildQualifiedName name
  LocalTempOptTempTableName showTable name -> P.LocalTempOptTempTableName showTable $ buildQualifiedName name
  GlobalTemporaryOptTempTableName showTable name -> P.GlobalTemporaryOptTempTableName showTable $ buildQualifiedName name
  GlobalTempOptTempTableName showTable name -> P.GlobalTempOptTempTableName showTable $ buildQualifiedName name
  UnloggedOptTempTableName showTable name -> P.UnloggedOptTempTableName showTable $ buildQualifiedName name
  TableOptTempTableName name -> P.TableOptTempTableName $ buildQualifiedName name
  QualifiedOptTempTableName name -> P.QualifedOptTempTableName $ buildQualifiedName name

buildFromClause :: FromClause -> P.FromClause
buildFromClause = fmap buildTableRef

buildTableRef :: (IsTableRef a) => a -> P.TableRef
buildTableRef a = case toTableRef a of
  RelationExprTableRef expr alias tablesample ->
    P.RelationExprTableRef (buildRelationExpr expr) alias (buildTablesampleClause <$> tablesample)
  FuncTableRef lateral funcTable alias ->
    P.FuncTableRef lateral (buildFuncTable funcTable) (buildFuncAliasClause <$> alias)
  SelectTableRef lateral select alias ->
    P.SelectTableRef lateral (fst $ buildSelectWithParens select) alias
  JoinTableRef joined alias ->
    P.JoinTableRef (buildJoinedTable joined) alias

buildTablesampleClause :: TablesampleClause -> P.TablesampleClause
buildTablesampleClause = \case
  TablesampleClause funcName exprs repeatable ->
    P.TablesampleClause
      (buildFuncName funcName)
      (buildExprList exprs)
      (buildAExpr <$> repeatable)

buildFuncTable :: FuncTable -> P.FuncTable
buildFuncTable = \case
  FuncExprFuncTable expr ord -> P.FuncExprFuncTable (buildFuncExprWindowless expr) ord
  RowsFromFuncTable rows ord -> P.RowsFromFuncTable (buildRowsFromList rows) ord

buildFuncExprWindowless :: FuncExprWindowless -> P.FuncExprWindowless
buildFuncExprWindowless = \case
  ApplicationFuncExprWindowless application ->
    P.ApplicationFuncExprWindowless $ buildFuncApplication application
  CommonSubexprFuncExprWindowless subexpr ->
    P.CommonSubexprFuncExprWindowless $ buildFuncExprCommonSubexpr subexpr

buildRowsFromList :: RowsFromList -> P.RowsfromList
buildRowsFromList = fmap buildRowsFromItem

buildRowsFromItem :: RowsFromItem -> P.RowsfromItem
buildRowsFromItem = \case
  RowsFromItem expr cols -> P.RowsfromItem (buildFuncExprWindowless expr) (buildColDefList <$> cols)

buildColDefList :: ColDefList -> P.ColDefList
buildColDefList = buildTableFuncElementList

buildTableFuncElementList :: ColDefList -> P.TableFuncElementList
buildTableFuncElementList = fmap buildTableFuncElement

buildTableFuncElement :: TableFuncElement -> P.TableFuncElement
buildTableFuncElement = \case
  TableFuncElement col typename collate -> P.TableFuncElement col (buildTypename typename) collate

buildFuncAliasClause :: FuncAliasClause -> P.FuncAliasClause
buildFuncAliasClause = \case
  AliasFuncAliasClause aliasClause -> P.AliasFuncAliasClause aliasClause
  AsFuncAliasClause tableFuncElementList ->
    P.AsFuncAliasClause $ buildTableFuncElementList tableFuncElementList
  AsColIdFuncAliasClause colId tableFuncElementList ->
    P.AsColIdFuncAliasClause colId $ buildTableFuncElementList tableFuncElementList
  ColIdFuncAliasClause colId tableFuncElementList ->
    P.ColIdFuncAliasClause colId $ buildTableFuncElementList tableFuncElementList

buildJoinedTable :: JoinedTable -> P.JoinedTable
buildJoinedTable = \case
  InParensJoinedTable table -> P.InParensJoinedTable $ buildJoinedTable table
  MethJoinedTable meth lhs rhs ->
    P.MethJoinedTable
      (buildJoinMeth meth)
      (buildTableRef lhs)
      (buildTableRef rhs)

buildJoinMeth :: JoinMeth -> P.JoinMeth
buildJoinMeth = \case
  CrossJoinMeth -> P.CrossJoinMeth
  QualJoinMeth joinType qual -> P.QualJoinMeth joinType $ buildJoinQual qual
  NaturalJoinMeth joinType -> P.NaturalJoinMeth joinType

buildJoinQual :: JoinQual -> P.JoinQual
buildJoinQual = \case
  UsingJoinQual cols -> P.UsingJoinQual cols
  OnJoinQual expr -> P.OnJoinQual $ buildAExpr expr

buildGroupClause :: GroupClause -> P.GroupClause
buildGroupClause = fmap buildGroupByItem

buildGroupByItem :: GroupByItem -> P.GroupByItem
buildGroupByItem = \case
  ExprGroupByItem expr -> P.ExprGroupByItem $ buildAExpr expr
  EmptyGroupingSetGroupByItem -> P.EmptyGroupingSetGroupByItem
  RollupGroupByItem exprs -> P.RollupGroupByItem $ buildExprList exprs
  CubeGroupByItem exprs -> P.CubeGroupByItem $ buildExprList exprs
  GroupingSetsGroupByItem items -> P.GroupingSetsGroupByItem $ fmap buildGroupByItem items

buildWindowClause :: WindowClause -> P.WindowClause
buildWindowClause = fmap buildWindowDefinition

buildWindowDefinition :: WindowDefinition -> P.WindowDefinition
buildWindowDefinition = \case
  WindowDefinition ident spec -> P.WindowDefinition ident $ buildWindowSpecification spec

buildSortClause :: SortClause -> P.SortClause
buildSortClause = fmap buildSortBy

buildSortBy :: SortBy -> P.SortBy
buildSortBy = \case
  UsingSortBy expr op nullsOrder -> P.UsingSortBy (buildAExpr expr) op nullsOrder
  AscDescSortBy expr ascDesc nullsOrder ->
    P.AscDescSortBy (buildAExpr expr) ascDesc nullsOrder

buildSelectLimit :: SelectLimit -> P.SelectLimit
buildSelectLimit = \case
  LimitOffsetSelectLimit limit offset ->
    P.LimitOffsetSelectLimit (buildLimitClause limit) (buildOffsetClause offset)
  OffsetLimitSelectLimit offset limit ->
    P.OffsetLimitSelectLimit (buildOffsetClause offset) (buildLimitClause limit)
  LimitSelectLimit limit -> P.LimitSelectLimit $ buildLimitClause limit
  OffsetSelectLimit offset -> P.OffsetSelectLimit $ buildOffsetClause offset

buildLimitClause :: (IsLimitClause a) => a -> P.LimitClause
buildLimitClause a = case toLimitClause a of
  LimitLimitClause selectLimitValue expr ->
    P.LimitLimitClause
      (buildSelectLimitValue selectLimitValue)
      (buildAExpr <$> expr)
  FetchOnlyLimitClause firstOrNext selectFetchFirstValue rowOrRows ->
    P.FetchOnlyLimitClause
      firstOrNext
      (buildSelectFetchFirstValue <$> selectFetchFirstValue)
      rowOrRows

buildSelectLimitValue :: (IsSelectLimitValue a) => a -> P.SelectLimitValue
buildSelectLimitValue a = case toSelectLimitValue a of
  ExprSelectLimitValue expr -> P.ExprSelectLimitValue $ buildAExpr expr
  AllSelectLimitValue -> P.AllSelectLimitValue

buildSelectFetchFirstValue :: (IsSelectFetchFirstValue a) => a -> P.SelectFetchFirstValue
buildSelectFetchFirstValue a = case toSelectFetchFirstValue a of
  ExprSelectFetchFirstValue expr -> P.ExprSelectFetchFirstValue $ buildCExpr expr
  NumSelectFetchFirstValue plusOrMinus iOrF -> P.NumSelectFetchFirstValue plusOrMinus iOrF

buildOffsetClause :: (IsOffsetClause a) => a -> P.OffsetClause
buildOffsetClause a = case toOffsetClause a of
  ExprOffsetClause expr -> P.ExprOffsetClause $ buildAExpr expr
  FetchFirstOffsetClause selectFetchFirstValue rowOrRows ->
    P.FetchFirstOffsetClause
      (buildSelectFetchFirstValue selectFetchFirstValue)
      rowOrRows

buildForLockingClause :: ForLockingClause -> P.ForLockingClause
buildForLockingClause = \case
  ItemsForLockingClause items -> P.ItemsForLockingClause $ fmap buildForLockingItem items
  ReadOnlyForLockingClause -> P.ReadOnlyForLockingClause

buildForLockingItem :: ForLockingItem -> P.ForLockingItem
buildForLockingItem = \case
  ForLockingItem strength names nowaitOrSkip ->
    P.ForLockingItem strength ((fmap . fmap) buildQualifiedName names) nowaitOrSkip

buildInsertTarget :: InsertTarget -> P.InsertTarget
buildInsertTarget = \case
  InsertTarget name alias -> P.InsertTarget (buildQualifiedName name) alias

buildInsertRest :: InsertRest -> P.InsertRest
buildInsertRest = \case
  SelectInsertRest columns override select ->
    P.SelectInsertRest
      (buildInsertColumnList <$> columns)
      override
      (fst $ buildSelectStmt select)
  DefaultValuesInsertRest -> P.DefaultValuesInsertRest

buildInsertColumnList :: InsertColumnList -> P.InsertColumnList
buildInsertColumnList = fmap buildInsertColumnItem

buildInsertColumnItem :: InsertColumnItem -> P.InsertColumnItem
buildInsertColumnItem = \case
  InsertColumnItem col indirection -> P.InsertColumnItem col $ fmap buildIndirection indirection

buildOnConflict :: OnConflict -> P.OnConflict
buildOnConflict = \case
  OnConflict confExpr doExpr -> P.OnConflict (buildConfExpr <$> confExpr) (buildOnConflictDo doExpr)

buildConfExpr :: ConfExpr -> P.ConfExpr
buildConfExpr = \case
  WhereConfExpr params whereClause ->
    P.WhereConfExpr
      (buildIndexParams params)
      (buildAExpr <$> whereClause)
  ConstraintConfExpr name -> P.ConstraintConfExpr name

buildIndexParams :: IndexParams -> P.IndexParams
buildIndexParams = fmap buildIndexElem

buildIndexElem :: IndexElem -> P.IndexElem
buildIndexElem = \case
  IndexElem elemDef collate class_ ascDesc nullsOrder -> do
    let elemDef' = buildIndexElemDef elemDef
    P.IndexElem elemDef' collate class_ ascDesc nullsOrder

buildIndexElemDef :: (IsIndexElemDef a) => a -> P.IndexElemDef
buildIndexElemDef a = case toIndexElemDef a of
  IdIndexElemDef col -> P.IdIndexElemDef col
  FuncIndexElemDef expr -> P.FuncIndexElemDef $ buildFuncExprWindowless expr
  ExprIndexElemDef expr -> P.ExprIndexElemDef $ buildAExpr expr

buildOnConflictDo :: OnConflictDo -> P.OnConflictDo
buildOnConflictDo = \case
  UpdateOnConflictDo setClauses whereClause ->
    P.UpdateOnConflictDo (buildSetClauseList setClauses) (buildAExpr <$> whereClause)
  NothingOnConflictDo -> P.NothingOnConflictDo

buildSetClauseList :: SetClauseList -> P.SetClauseList
buildSetClauseList = fmap buildSetClause

buildSetClause :: SetClause -> P.SetClause
buildSetClause = \case
  TargetSetClause target expr -> P.TargetSetClause (buildSetTarget target) (buildAExpr expr)
  TargetListSetClause targets expr -> P.TargetListSetClause (buildSetTargetList targets) (buildAExpr expr)

buildSetTarget :: SetTarget -> P.SetTarget
buildSetTarget = \case
  SetTarget col indirection -> P.SetTarget col $ buildIndirection <$> indirection

buildSetTargetList :: SetTargetList -> P.SetTargetList
buildSetTargetList = fmap buildSetTarget

buildReturningClause :: ReturningClause row -> (Maybe P.ReturningClause, TargetListDecoders row)
buildReturningClause = \case
  EmptyReturningClause -> (Nothing, ListDecodersNil)
  TargetListReturningClause targets -> first Just $ buildTargetList targets

buildRelationExprOptAlias :: RelationExprOptAlias -> P.RelationExprOptAlias
buildRelationExprOptAlias = \case
  RelationExprOptAlias expr alias -> flip P.RelationExprOptAlias alias $ buildRelationExpr expr

buildWhereOrCurrentClause :: WhereOrCurrentClause -> P.WhereOrCurrentClause
buildWhereOrCurrentClause = \case
  ExprWhereOrCurrentClause expr -> P.ExprWhereOrCurrentClause $ buildAExpr expr
  CursorWhereOrCurrentClause name -> P.CursorWhereOrCurrentClause name

buildUsingClause :: UsingClause -> P.UsingClause
buildUsingClause = buildFromList

buildFromList :: FromList -> P.FromList
buildFromList = fmap buildTableRef

data TargetDecoders row where
  DecodersNil :: TargetDecoders '[]
  DecodersCons :: Decoders.Row (ColumnToHask t) -> TargetDecoders row -> TargetDecoders (t ': row)

data TargetListDecoders rows where
  ListDecodersNil :: TargetListDecoders '[]
  ListDecodersCons :: TargetDecoders (t ': row) -> TargetListDecoders rows -> TargetListDecoders ((t ': row) ': rows)

data TargetListResult rows where
  ListResultNil :: TargetListResult '[]
  ListResultCons :: TargetResult row -> TargetListResult rows -> TargetListResult (row ': rows)

data TargetResult row where
  ResultNil :: TargetResult '[]
  ResultCons :: ColumnToHask t -> TargetResult row -> TargetResult (t ': row)

compileListDecoders :: TargetListDecoders rows -> Decoders.Row (TargetListResult rows)
compileListDecoders ListDecodersNil = pure ListResultNil
compileListDecoders (ListDecodersCons row decoders) = ListResultCons <$> compileDecoders row <*> compileListDecoders decoders

compileDecoders :: TargetDecoders row -> Decoders.Row (TargetResult row)
compileDecoders DecodersNil = pure ResultNil
compileDecoders (DecodersCons row decoders) = ResultCons <$> row <*> compileDecoders decoders
