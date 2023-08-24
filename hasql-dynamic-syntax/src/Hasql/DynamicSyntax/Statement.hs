{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Hasql.DynamicSyntax.Statement (
  buildStatement,
) where

import Control.Monad.Trans.RWS (RWS, runRWS, rws)
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import qualified Data.List.NonEmpty as NE
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicSyntax.Ast
import Hasql.Encoders (Params)
import Hasql.Statement (Statement (..))
import qualified PostgresqlSyntax.Ast as P
import qualified PostgresqlSyntax.Rendering as Render

buildStatement :: (Decoders.Row (RowResult row) -> Decoders.Result a) -> PreparableStmt row -> Statement () a
buildStatement toResult stmt = Statement sql params (toResult $ compileDecoders row) (paramCount > 0)
  where
    ((stmt', row), paramCount, params) = runRWS (runBuildM $ buildPreparableStmt stmt) () 0
    sql = Render.toByteString $ Render.preparableStmt stmt'

newtype BuildM a = BuildM {runBuildM :: RWS () (Params ()) Int a}
  deriving newtype (Functor, Applicative, Monad)

buildPreparableStmt :: PreparableStmt row -> BuildM (P.PreparableStmt, RowDecoders row)
buildPreparableStmt = \case
  SelectPreparableStmt stmt -> first P.SelectPreparableStmt <$> buildSelectStmt stmt
  InsertPreparableStmt stmt -> first P.InsertPreparableStmt <$> buildInsertStmt stmt
  UpdatePreparableStmt stmt -> first P.UpdatePreparableStmt <$> buildUpdateStmt stmt
  DeletePreparableStmt stmt -> first P.DeletePreparableStmt <$> buildDeleteStmt stmt
  CallPreparableStmt stmt -> first P.CallPreparableStmt <$> buildCallStmt stmt

buildSelectStmt :: SelectStmt row -> BuildM (P.SelectStmt, RowDecoders row)
buildSelectStmt = \case
  Left s -> first Left <$> buildSelectNoParens s
  Right s -> first Right <$> buildSelectWithParens s

buildInsertStmt :: InsertStmt row -> BuildM (P.InsertStmt, RowDecoders row)
buildInsertStmt = \case
  InsertStmt with target rest onConflict returning -> do
    with' <- traverse buildWithClause with
    target' <- buildInsertTarget target
    rest' <- buildInsertRest rest
    onConflict' <- traverse buildOnConflict onConflict
    (returning', row) <- buildReturningClause returning
    pure (P.InsertStmt with' target' rest' onConflict' returning', row)

buildUpdateStmt :: UpdateStmt row -> BuildM (P.UpdateStmt, RowDecoders row)
buildUpdateStmt = \case
  UpdateStmt with rel setClauseList from whereOrCurrent returning -> do
    with' <- traverse buildWithClause with
    rel' <- buildRelationExprOptAlias rel
    setClauseList' <- buildSetClauseList setClauseList
    from' <- traverse buildFromClause from
    whereOrCurrent' <- traverse buildWhereOrCurrentClause whereOrCurrent
    (returning', row) <- buildReturningClause returning
    pure (P.UpdateStmt with' rel' setClauseList' from' whereOrCurrent' returning', row)

buildDeleteStmt :: DeleteStmt row -> BuildM (P.DeleteStmt, RowDecoders row)
buildDeleteStmt = \case
  DeleteStmt with rel usingClause whereOrCurrent returning -> do
    with' <- traverse buildWithClause with
    rel' <- buildRelationExprOptAlias rel
    usingClause' <- traverse buildUsingClause usingClause
    whereOrCurrent' <- traverse buildWhereOrCurrentClause whereOrCurrent
    (returning', row) <- buildReturningClause returning
    pure (P.DeleteStmt with' rel' usingClause' whereOrCurrent' returning', row)

buildCallStmt :: CallStmt row -> BuildM (P.CallStmt, RowDecoders row)
buildCallStmt = \case
  CallStmt row funcApplication -> (,rowDecoders row) . P.CallStmt <$> buildFuncApplication funcApplication

buildSelectWithParens :: SelectWithParens row -> BuildM (P.SelectWithParens, RowDecoders row)
buildSelectWithParens = \case
  NoParensSelectWithParens s -> first P.NoParensSelectWithParens <$> buildSelectNoParens s
  WithParensSelectWithParens s -> first P.WithParensSelectWithParens <$> buildSelectWithParens s

buildSelectNoParens :: SelectNoParens row -> BuildM (P.SelectNoParens, RowDecoders row)
buildSelectNoParens = \case
  SelectNoParens with select sort limit forLocking -> do
    with' <- traverse buildWithClause with
    (select', row) <- buildSelectClause select
    sort' <- traverse buildSortClause sort
    limit' <- traverse buildSelectLimit limit
    forLocking' <- traverse buildForLockingClause forLocking
    pure (P.SelectNoParens with' select' sort' limit' forLocking', row)

buildWithClause :: WithClause -> BuildM P.WithClause
buildWithClause = \case
  WithClause rec ctes -> P.WithClause rec <$> traverse buildCommonTableExpr ctes

buildCommonTableExpr :: CommonTableExpr -> BuildM P.CommonTableExpr
buildCommonTableExpr = \case
  CommonTableExpr name cols materialized stmt ->
    P.CommonTableExpr name cols materialized . fst <$> buildPreparableStmt stmt

buildSelectClause :: SelectClause row -> BuildM (P.SelectClause, RowDecoders row)
buildSelectClause = \case
  Left s -> first Left <$> buildSimpleSelect s
  Right s -> first Right <$> buildSelectWithParens s

buildSimpleSelect :: SimpleSelect row -> BuildM (P.SimpleSelect, RowDecoders row)
buildSimpleSelect = \case
  NormalSimpleSelect targeting into from where_ group having window -> do
    (targeting', row) <- buildTargeting targeting
    into' <- traverse buildIntoClause into
    from' <- traverse buildFromClause from
    where_' <- traverse buildWhereClause where_
    group' <- traverse buildGroupClause group
    having' <- traverse buildHavingClause having
    window' <- traverse buildWindowClause window
    pure (P.NormalSimpleSelect targeting' into' from' where_' group' having' window', row)
  ValuesSimpleSelect row values ->
    (,rowDecoders row) . P.ValuesSimpleSelect <$> buildValuesClause values
  TableSimpleSelect row rel ->
    (,rowDecoders row) . P.TableSimpleSelect <$> buildRelationExpr rel
  BinSimpleSelect op lhs allOrDistinct rhs -> do
    (lhs', row) <- buildSelectClause lhs
    (rhs', _) <- buildSelectClause rhs
    pure (P.BinSimpleSelect op lhs' allOrDistinct rhs', row)

buildRelationExpr :: RelationExpr -> BuildM P.RelationExpr
buildRelationExpr = \case
  SimpleRelationExpr name asterisk -> P.SimpleRelationExpr <$> buildQualifiedName name <*> pure asterisk
  OnlyRelationExpr name parens -> P.OnlyRelationExpr <$> buildQualifiedName name <*> pure parens

buildQualifiedName :: QualifiedName -> BuildM P.QualifiedName
buildQualifiedName = \case
  SimpleQualifiedName ident -> pure $ P.SimpleQualifiedName ident
  IndirectedQualifiedName ident indirection -> P.IndirectedQualifiedName ident <$> buildIndirection indirection

buildIndirection :: Indirection -> BuildM P.Indirection
buildIndirection = traverse buildIndirectionEl

buildIndirectionEl :: IndirectionEl -> BuildM P.IndirectionEl
buildIndirectionEl = \case
  AttrNameIndirectionEl ident -> pure $ P.AttrNameIndirectionEl ident
  AllIndirectionEl -> pure P.AllIndirectionEl
  ExprIndirectionEl expr -> P.ExprIndirectionEl <$> buildAExpr expr
  SliceIndirectionEl lowBound hiBound -> P.SliceIndirectionEl <$> traverse buildAExpr lowBound <*> traverse buildAExpr hiBound

buildAExpr :: AExpr -> BuildM P.AExpr
buildAExpr = \case
  CExprAExpr expr -> P.CExprAExpr <$> buildCExpr expr
  TypecastAExpr expr ty -> P.TypecastAExpr <$> buildAExpr expr <*> buildTypename ty
  CollateAExpr expr collation -> P.CollateAExpr <$> buildAExpr expr <*> pure collation
  AtTimeZoneAExpr lhs rhs -> P.AtTimeZoneAExpr <$> buildAExpr lhs <*> buildAExpr rhs
  PlusAExpr expr -> P.PlusAExpr <$> buildAExpr expr
  MinusAExpr expr -> P.MinusAExpr <$> buildAExpr expr
  SymbolicBinOpAExpr lhs op rhs -> P.SymbolicBinOpAExpr <$> buildAExpr lhs <*> pure op <*> buildAExpr rhs
  PrefixQualOpAExpr op expr -> P.PrefixQualOpAExpr op <$> buildAExpr expr
  SuffixQualOpAExpr expr op -> flip P.SuffixQualOpAExpr op <$> buildAExpr expr
  AndAExpr lhs rhs -> P.AndAExpr <$> buildAExpr lhs <*> buildAExpr rhs
  OrAExpr lhs rhs -> P.OrAExpr <$> buildAExpr lhs <*> buildAExpr rhs
  NotAExpr expr -> P.NotAExpr <$> buildAExpr expr
  VerbalExprBinOpAExpr lhs not_ op rhs escape ->
    P.VerbalExprBinOpAExpr
      <$> buildAExpr lhs
      <*> pure not_
      <*> pure op
      <*> buildAExpr rhs
      <*> traverse buildAExpr escape
  ReversableOpAExpr expr not_ op ->
    P.ReversableOpAExpr <$> buildAExpr expr <*> pure not_ <*> buildAExprReversableOp op
  IsnullAExpr expr -> P.IsnullAExpr <$> buildAExpr expr
  NotnullAExpr expr -> P.NotnullAExpr <$> buildAExpr expr
  OverlapsAExpr lhs rhs -> P.OverlapsAExpr <$> buildRow lhs <*> buildRow rhs
  SubqueryAExpr expr op subtype sub ->
    P.SubqueryAExpr
      <$> buildAExpr expr
      <*> pure op
      <*> pure subtype
      <*> bitraverse (fmap fst . buildSelectWithParens) buildAExpr sub
  UniqueAExpr select -> P.UniqueAExpr . fst <$> buildSelectWithParens select
  DefaultAExpr -> pure P.DefaultAExpr

buildCExpr :: CExpr -> BuildM P.CExpr
buildCExpr = \case
  ColumnrefCExpr col -> P.ColumnrefCExpr <$> buildColumnref col
  AexprConstCExpr expr -> P.AexprConstCExpr <$> buildAExprConst expr
  ParamCExpr encoder -> BuildM $ rws \_ paramCount ->
    (P.ParamCExpr (paramCount + 1) Nothing, paramCount + 1, encoder)
  InParensCExpr expr indirection -> P.InParensCExpr <$> buildAExpr expr <*> traverse buildIndirection indirection
  CaseCExpr case_ -> P.CaseCExpr <$> buildCaseExpr case_
  FuncCExpr func -> P.FuncCExpr <$> buildFuncExpr func
  SelectWithParensCExpr select indirection ->
    P.SelectWithParensCExpr <$> (fst <$> buildSelectWithParens select) <*> traverse buildIndirection indirection
  ExistsCExpr select -> P.ExistsCExpr . fst <$> buildSelectWithParens select
  ArrayCExpr array -> P.ArrayCExpr <$> bitraverse (fmap fst . buildSelectWithParens) buildArrayExpr array
  ExplicitRowCExpr row -> P.ExplicitRowCExpr <$> buildExplicitRow row
  ImplicitRowCExpr row -> P.ImplicitRowCExpr <$> buildImplicitRow row
  GroupingCExpr exprs -> P.GroupingCExpr <$> buildExprList exprs

buildColumnref :: Columnref -> BuildM P.Columnref
buildColumnref = \case
  Columnref col indirection -> P.Columnref col <$> traverse buildIndirection indirection

buildAExprConst :: AexprConst -> BuildM P.AexprConst
buildAExprConst = \case
  IAexprConst i -> pure $ P.IAexprConst i
  FAexprConst f -> pure $ P.FAexprConst f
  SAexprConst s -> pure $ P.SAexprConst s
  BAexprConst b -> pure $ P.BAexprConst b
  XAexprConst x -> pure $ P.XAexprConst x
  FuncAexprConst funcName args s ->
    P.FuncAexprConst
      <$> buildFuncName funcName
      <*> traverse buildFuncConstArgs args
      <*> pure s
  ConstTypenameAexprConst typename s -> P.ConstTypenameAexprConst <$> buildConstTypename typename <*> pure s
  StringIntervalAexprConst s interval -> pure $ P.StringIntervalAexprConst s interval
  IntIntervalAexprConst i s -> pure $ P.IntIntervalAexprConst i s
  BoolAexprConst b -> pure $ P.BoolAexprConst b
  NullAexprConst -> pure P.NullAexprConst

buildFuncName :: FuncName -> BuildM P.FuncName
buildFuncName = \case
  TypeFuncName name -> pure $ P.TypeFuncName name
  IndirectedFuncName col indirection -> P.IndirectedFuncName col <$> buildIndirection indirection

buildFuncConstArgs :: FuncConstArgs -> BuildM P.FuncConstArgs
buildFuncConstArgs = \case
  FuncConstArgs args sort -> P.FuncConstArgs <$> traverse buildFuncArgExpr args <*> traverse buildSortClause sort

buildFuncArgExpr :: FuncArgExpr -> BuildM P.FuncArgExpr
buildFuncArgExpr = \case
  ExprFuncArgExpr expr -> P.ExprFuncArgExpr <$> buildAExpr expr
  ColonEqualsFuncArgExpr ident expr -> P.ColonEqualsFuncArgExpr ident <$> buildAExpr expr
  EqualsGreaterFuncArgExpr ident expr -> P.EqualsGreaterFuncArgExpr ident <$> buildAExpr expr

buildConstTypename :: ConstTypename -> BuildM P.ConstTypename
buildConstTypename = \case
  NumericConstTypename numeric -> P.NumericConstTypename <$> buildNumeric numeric
  ConstBitConstTypename constBit -> P.ConstBitConstTypename <$> buildConstBit constBit
  ConstCharacterConstTypename constChar -> pure $ P.ConstCharacterConstTypename constChar
  ConstDatetimeConstTypename constDatetime -> pure $ P.ConstDatetimeConstTypename constDatetime

buildNumeric :: Numeric -> BuildM P.Numeric
buildNumeric = \case
  IntNumeric -> pure P.IntNumeric
  IntegerNumeric -> pure P.IntegerNumeric
  SmallintNumeric -> pure P.SmallintNumeric
  BigintNumeric -> pure P.BigintNumeric
  RealNumeric -> pure P.RealNumeric
  FloatNumeric float -> pure $ P.FloatNumeric float
  DoublePrecisionNumeric -> pure P.DoublePrecisionNumeric
  DecimalNumeric mods -> P.DecimalNumeric <$> traverse buildTypeModifiers mods
  DecNumeric mods -> P.DecNumeric <$> traverse buildTypeModifiers mods
  NumericNumeric mods -> P.NumericNumeric <$> traverse buildTypeModifiers mods
  BooleanNumeric -> pure P.BooleanNumeric

buildTypeModifiers :: TypeModifiers -> BuildM P.TypeModifiers
buildTypeModifiers = buildExprList

buildConstBit :: ConstBit -> BuildM P.ConstBit
buildConstBit = buildBit

buildBit :: ConstBit -> BuildM P.ConstBit
buildBit = \case
  Bit varying exprs -> P.Bit varying <$> traverse buildExprList exprs

buildCaseExpr :: CaseExpr -> BuildM P.CaseExpr
buildCaseExpr = \case
  CaseExpr arg clauseList def ->
    P.CaseExpr
      <$> traverse buildCaseArg arg
      <*> buildWhenClauseList clauseList
      <*> traverse buildCaseDefault def

buildCaseArg :: CaseArg -> BuildM P.CaseArg
buildCaseArg = buildAExpr

buildWhenClauseList :: WhenClauseList -> BuildM P.WhenClauseList
buildWhenClauseList = traverse buildWhenClause

buildWhenClause :: WhenClause -> BuildM P.WhenClause
buildWhenClause = \case
  WhenClause a b -> P.WhenClause <$> buildAExpr a <*> buildAExpr b

buildCaseDefault :: CaseDefault -> BuildM P.CaseDefault
buildCaseDefault = buildAExpr

buildFuncExpr :: FuncExpr -> BuildM P.FuncExpr
buildFuncExpr = \case
  ApplicationFuncExpr application withinGroup filter_ over ->
    P.ApplicationFuncExpr
      <$> buildFuncApplication application
      <*> traverse buildWithinGroupClause withinGroup
      <*> traverse buildFilterClause filter_
      <*> traverse buildOverClause over
  SubexprFuncExpr subexpr -> P.SubexprFuncExpr <$> buildFuncExprCommonSubexpr subexpr

buildFuncExprCommonSubexpr :: FuncExprCommonSubexpr -> BuildM P.FuncExprCommonSubexpr
buildFuncExprCommonSubexpr = \case
  CollationForFuncExprCommonSubexpr expr -> P.CollationForFuncExprCommonSubexpr <$> buildAExpr expr
  CurrentDateFuncExprCommonSubexpr -> pure P.CurrentDateFuncExprCommonSubexpr
  CurrentTimeFuncExprCommonSubexpr i -> pure $ P.CurrentTimeFuncExprCommonSubexpr i
  CurrentTimestampFuncExprCommonSubexpr i -> pure $ P.CurrentTimestampFuncExprCommonSubexpr i
  LocalTimeFuncExprCommonSubexpr i -> pure $ P.LocalTimeFuncExprCommonSubexpr i
  LocalTimestampFuncExprCommonSubexpr i -> pure $ P.LocalTimestampFuncExprCommonSubexpr i
  CurrentRoleFuncExprCommonSubexpr -> pure P.CurrentRoleFuncExprCommonSubexpr
  CurrentUserFuncExprCommonSubexpr -> pure P.CurrentUserFuncExprCommonSubexpr
  SessionUserFuncExprCommonSubexpr -> pure P.SessionUserFuncExprCommonSubexpr
  UserFuncExprCommonSubexpr -> pure P.UserFuncExprCommonSubexpr
  CurrentCatalogFuncExprCommonSubexpr -> pure P.CurrentCatalogFuncExprCommonSubexpr
  CurrentSchemaFuncExprCommonSubexpr -> pure P.CurrentSchemaFuncExprCommonSubexpr
  CastFuncExprCommonSubexpr expr typename ->
    P.CastFuncExprCommonSubexpr
      <$> buildAExpr expr
      <*> buildTypename typename
  ExtractFuncExprCommonSubexpr extracts -> P.ExtractFuncExprCommonSubexpr <$> traverse buildExtractList extracts
  OverlayFuncExprCommonSubexpr overlays -> P.OverlayFuncExprCommonSubexpr <$> buildOverlayList overlays
  PositionFuncExprCommonSubexpr positions -> P.PositionFuncExprCommonSubexpr <$> traverse buildPositionList positions
  SubstringFuncExprCommonSubexpr substrs -> P.SubstringFuncExprCommonSubexpr <$> traverse buildSubstrList substrs
  TreatFuncExprCommonSubexpr expr typename ->
    P.TreatFuncExprCommonSubexpr
      <$> buildAExpr expr
      <*> buildTypename typename
  TrimFuncExprCommonSubexpr trimMod trimList -> P.TrimFuncExprCommonSubexpr trimMod <$> buildTrimList trimList
  NullIfFuncExprCommonSubexpr lhs rhs -> P.NullIfFuncExprCommonSubexpr <$> buildAExpr lhs <*> buildAExpr rhs
  CoalesceFuncExprCommonSubexpr exprs -> P.CoalesceFuncExprCommonSubexpr <$> buildExprList exprs
  GreatestFuncExprCommonSubexpr exprs -> P.GreatestFuncExprCommonSubexpr <$> buildExprList exprs
  LeastFuncExprCommonSubexpr exprs -> P.LeastFuncExprCommonSubexpr <$> buildExprList exprs

buildExtractList :: ExtractList -> BuildM P.ExtractList
buildExtractList = \case
  ExtractList arg expr -> P.ExtractList arg <$> buildAExpr expr

buildOverlayList :: OverlayList -> BuildM P.OverlayList
buildOverlayList = \case
  OverlayList expr placing substrFrom substrFor ->
    P.OverlayList
      <$> buildAExpr expr
      <*> buildOverlayPlacing placing
      <*> buildSubstrFrom substrFrom
      <*> traverse buildSubstrFor substrFor

buildOverlayPlacing :: OverlayPlacing -> BuildM P.OverlayPlacing
buildOverlayPlacing = buildAExpr

buildSubstrFrom :: SubstrFrom -> BuildM P.SubstrFrom
buildSubstrFrom = buildAExpr

buildSubstrFor :: SubstrFor -> BuildM P.SubstrFor
buildSubstrFor = buildAExpr

buildPositionList :: PositionList -> BuildM P.PositionList
buildPositionList = \case
  PositionList lhs rhs -> P.PositionList <$> buildBExpr lhs <*> buildBExpr rhs

buildSubstrList :: SubstrList -> BuildM P.SubstrList
buildSubstrList = \case
  ExprSubstrList expr fromFor -> P.ExprSubstrList <$> buildAExpr expr <*> buildSubstrListFromFor fromFor
  ExprListSubstrList exprs -> P.ExprListSubstrList <$> buildExprList exprs

buildSubstrListFromFor :: SubstrListFromFor -> BuildM P.SubstrListFromFor
buildSubstrListFromFor = \case
  FromForSubstrListFromFor from for -> P.FromForSubstrListFromFor <$> buildSubstrFrom from <*> buildSubstrFor for
  ForFromSubstrListFromFor for from -> P.ForFromSubstrListFromFor <$> buildSubstrFor for <*> buildSubstrFrom from
  FromSubstrListFromFor from -> P.FromSubstrListFromFor <$> buildSubstrFrom from
  ForSubstrListFromFor for -> P.ForSubstrListFromFor <$> buildSubstrFor for

buildTrimList :: TrimList -> BuildM P.TrimList
buildTrimList = \case
  ExprFromExprListTrimList expr exprs -> P.ExprFromExprListTrimList <$> buildAExpr expr <*> buildExprList exprs
  FromExprListTrimList exprs -> P.FromExprListTrimList <$> buildExprList exprs
  ExprListTrimList exprs -> P.ExprListTrimList <$> buildExprList exprs

buildFuncApplication :: FuncApplication -> BuildM P.FuncApplication
buildFuncApplication = \case
  FuncApplication name params ->
    P.FuncApplication <$> buildFuncName name <*> traverse buildFuncApplicationParams params

buildFuncApplicationParams :: FuncApplicationParams -> BuildM P.FuncApplicationParams
buildFuncApplicationParams = \case
  NormalFuncApplicationParams allOrDistinct args sort ->
    P.NormalFuncApplicationParams allOrDistinct
      <$> traverse buildFuncArgExpr args
      <*> traverse buildSortClause sort
  VariadicFuncApplicationParams args arg sort ->
    P.VariadicFuncApplicationParams
      <$> traverse (traverse buildFuncArgExpr) args
      <*> buildFuncArgExpr arg
      <*> traverse buildSortClause sort
  StarFuncApplicationParams -> pure P.StarFuncApplicationParams

buildWithinGroupClause :: WithinGroupClause -> BuildM P.WithinGroupClause
buildWithinGroupClause = buildSortClause

buildFilterClause :: FilterClause -> BuildM P.FilterClause
buildFilterClause = buildAExpr

buildOverClause :: OverClause -> BuildM P.OverClause
buildOverClause = \case
  WindowOverClause window -> P.WindowOverClause <$> buildWindowSpecification window
  ColIdOverClause col -> pure $ P.ColIdOverClause col

buildWindowSpecification :: WindowSpecification -> BuildM P.WindowSpecification
buildWindowSpecification = \case
  WindowSpecification name partition sort frame ->
    P.WindowSpecification name
      <$> traverse buildExprList partition
      <*> traverse buildSortClause sort
      <*> traverse buildFrameClause frame

buildFrameClause :: FrameClause -> BuildM P.FrameClause
buildFrameClause = \case
  FrameClause mode extent exclusion -> P.FrameClause mode <$> buildFrameExtent extent <*> pure exclusion

buildFrameExtent :: FrameExtent -> BuildM P.FrameExtent
buildFrameExtent = \case
  SingularFrameExtent b -> P.SingularFrameExtent <$> buildFrameBound b
  BetweenFrameExtent b b' -> P.BetweenFrameExtent <$> buildFrameBound b <*> buildFrameBound b'

buildFrameBound :: FrameBound -> BuildM P.FrameBound
buildFrameBound = \case
  UnboundedPrecedingFrameBound -> pure P.UnboundedFollowingFrameBound
  UnboundedFollowingFrameBound -> pure P.UnboundedFollowingFrameBound
  CurrentRowFrameBound -> pure P.CurrentRowFrameBound
  PrecedingFrameBound expr -> P.PrecedingFrameBound <$> buildAExpr expr
  FollowingFrameBound expr -> P.FollowingFrameBound <$> buildAExpr expr

buildArrayExpr :: ArrayExpr -> BuildM P.ArrayExpr
buildArrayExpr = \case
  ExprListArrayExpr exprs -> P.ExprListArrayExpr <$> buildExprList exprs
  ArrayExprListArrayExpr exprs -> P.ArrayExprListArrayExpr <$> buildArrayExprList exprs
  EmptyArrayExpr -> pure P.EmptyArrayExpr

buildArrayExprList :: ArrayExprList -> BuildM P.ArrayExprList
buildArrayExprList = traverse buildArrayExpr

buildExplicitRow :: ExplicitRow -> BuildM P.ExplicitRow
buildExplicitRow = traverse buildExprList

buildImplicitRow :: ImplicitRow -> BuildM P.ImplicitRow
buildImplicitRow = \case
  ImplicitRow exprs expr -> P.ImplicitRow <$> buildExprList exprs <*> buildAExpr expr

buildExprList :: ExprList -> BuildM P.ExprList
buildExprList = traverse buildAExpr

buildTypename :: Typename -> BuildM P.Typename
buildTypename = \case
  Typename setof name questionMark arrayBounds ->
    P.Typename setof <$> buildSimpleTypename name <*> pure questionMark <*> pure arrayBounds

buildSimpleTypename :: SimpleTypename -> BuildM P.SimpleTypename
buildSimpleTypename = \case
  GenericTypeSimpleTypename ty -> P.GenericTypeSimpleTypename <$> buildGenericType ty
  NumericSimpleTypename n -> P.NumericSimpleTypename <$> buildNumeric n
  BitSimpleTypename b -> P.BitSimpleTypename <$> buildBit b
  CharacterSimpleTypename ch -> pure $ P.CharacterSimpleTypename ch
  ConstDatetimeSimpleTypename dt -> pure $ P.ConstDatetimeSimpleTypename dt
  ConstIntervalSimpleTypename iv -> pure $ P.ConstIntervalSimpleTypename iv

buildGenericType :: GenericType -> BuildM P.GenericType
buildGenericType = \case
  GenericType name attrs mods ->
    P.GenericType name attrs <$> traverse buildTypeModifiers mods

buildAExprReversableOp :: AExprReversableOp -> BuildM P.AExprReversableOp
buildAExprReversableOp = \case
  NullAExprReversableOp -> pure P.NullAExprReversableOp
  TrueAExprReversableOp -> pure P.TrueAExprReversableOp
  FalseAExprReversableOp -> pure P.FalseAExprReversableOp
  UnknownAExprReversableOp -> pure P.UnknownAExprReversableOp
  DistinctFromAExprReversableOp expr -> P.DistinctFromAExprReversableOp <$> buildAExpr expr
  OfAExprReversableOp typeList -> P.OfAExprReversableOp <$> buildTypeList typeList
  BetweenAExprReversableOp asymmetric lhs rhs ->
    P.BetweenAExprReversableOp asymmetric <$> buildBExpr lhs <*> buildAExpr rhs
  BetweenSymmetricAExprReversableOp lhs rhs ->
    P.BetweenSymmetricAExprReversableOp <$> buildBExpr lhs <*> buildAExpr rhs
  InAExprReversableOp expr -> P.InAExprReversableOp <$> buildInExpr expr
  DocumentAExprReversableOp -> pure P.DocumentAExprReversableOp

buildInExpr :: InExpr -> BuildM P.InExpr
buildInExpr = \case
  SelectInExpr select -> P.SelectInExpr . fst <$> buildSelectWithParens select
  ExprListInExpr exprs -> P.ExprListInExpr <$> buildExprList exprs

buildBExpr :: BExpr -> BuildM P.BExpr
buildBExpr = \case
  CExprBExpr expr -> P.CExprBExpr <$> buildCExpr expr
  TypecastBExpr expr typename -> P.TypecastBExpr <$> buildBExpr expr <*> buildTypename typename
  PlusBExpr expr -> P.PlusBExpr <$> buildBExpr expr
  MinusBExpr expr -> P.MinusBExpr <$> buildBExpr expr
  SymbolicBinOpBExpr lhs op rhs -> P.SymbolicBinOpBExpr <$> buildBExpr lhs <*> pure op <*> buildBExpr rhs
  QualOpBExpr op expr -> P.QualOpBExpr op <$> buildBExpr expr
  IsOpBExpr expr not_ op -> P.IsOpBExpr <$> buildBExpr expr <*> pure not_ <*> buildBExprIsOp op

buildBExprIsOp :: BExprIsOp -> BuildM P.BExprIsOp
buildBExprIsOp = \case
  DistinctFromBExprIsOp expr -> P.DistinctFromBExprIsOp <$> buildBExpr expr
  OfBExprIsOp typeList -> P.OfBExprIsOp <$> buildTypeList typeList
  DocumentBExprIsOp -> pure P.DocumentBExprIsOp

buildTypeList :: TypeList -> BuildM P.TypeList
buildTypeList = traverse buildTypename

buildRow :: Row -> BuildM P.Row
buildRow = \case
  ExplicitRowRow r -> P.ExplicitRowRow <$> buildExplicitRow r
  ImplicitRowRow r -> P.ImplicitRowRow <$> buildImplicitRow r

buildValuesClause :: ValuesClause -> BuildM P.ValuesClause
buildValuesClause = traverse buildExprList

rowDecoders :: DeclareRow row -> RowDecoders row
rowDecoders = \case
  DeclareNil -> DecodersNil
  DeclareCons sqlType row -> DecodersCons (Decoders.column $ columnDecoder sqlType) $ rowDecoders row

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

buildTargeting :: Targeting row -> BuildM (Maybe P.Targeting, RowDecoders row)
buildTargeting = \case
  EmptyTargeting -> pure (Nothing, DecodersNil)
  NormalTargeting targets -> first (Just . P.NormalTargeting) <$> buildTargetList targets
  AllTargeting targets -> first (Just . P.NormalTargeting) <$> buildTargetList targets
  DistinctTargeting exprs targets -> do
    exprs' <- traverse buildExprList exprs
    first (Just . P.DistinctTargeting exprs') <$> buildTargetList targets

buildTargetList :: TargetList row -> BuildM (P.TargetList, RowDecoders row)
buildTargetList = \case
  TargetListOne target -> first pure <$> buildTargetElRow target
  TargetListCons target list -> do
    (target', row) <- buildTargetElRow target
    (list', row') <- buildTargetList list
    pure (NE.cons target' list', row `appendRowDecoders` row')

buildTargetElRow :: TargetElRow row -> BuildM (P.TargetEl, RowDecoders row)
buildTargetElRow = \case
  TargetElRow row target -> (,rowDecoders row) <$> buildTargetEl target

buildTargetEl :: TargetEl -> BuildM P.TargetEl
buildTargetEl = \case
  AliasedExprTargetEl expr alias -> P.AliasedExprTargetEl <$> buildAExpr expr <*> pure alias
  ImplicitlyAliasedExprTargetEl expr alias -> P.ImplicitlyAliasedExprTargetEl <$> buildAExpr expr <*> pure alias
  ExprTargetEl expr -> P.ExprTargetEl <$> buildAExpr expr
  AsteriskTargetEl -> pure P.AsteriskTargetEl

buildIntoClause :: IntoClause -> BuildM P.IntoClause
buildIntoClause = \case
  TemporaryOptTempTableName showTable name -> P.TemporaryOptTempTableName showTable <$> buildQualifiedName name
  TempOptTempTableName showTable name -> P.TempOptTempTableName showTable <$> buildQualifiedName name
  LocalTemporaryOptTempTableName showTable name -> P.LocalTemporaryOptTempTableName showTable <$> buildQualifiedName name
  LocalTempOptTempTableName showTable name -> P.LocalTempOptTempTableName showTable <$> buildQualifiedName name
  GlobalTemporaryOptTempTableName showTable name -> P.GlobalTemporaryOptTempTableName showTable <$> buildQualifiedName name
  GlobalTempOptTempTableName showTable name -> P.GlobalTempOptTempTableName showTable <$> buildQualifiedName name
  UnloggedOptTempTableName showTable name -> P.UnloggedOptTempTableName showTable <$> buildQualifiedName name
  TableOptTempTableName name -> P.TableOptTempTableName <$> buildQualifiedName name
  QualifiedOptTempTableName name -> P.QualifedOptTempTableName <$> buildQualifiedName name

buildFromClause :: FromClause -> BuildM P.FromClause
buildFromClause = traverse buildTableRef

buildTableRef :: TableRef -> BuildM P.TableRef
buildTableRef = \case
  RelationExprTableRef expr alias tablesample ->
    P.RelationExprTableRef <$> buildRelationExpr expr <*> pure alias <*> traverse buildTablesampleClause tablesample
  FuncTableRef lateral funcTable alias ->
    P.FuncTableRef lateral <$> buildFuncTable funcTable <*> traverse buildFuncAliasClause alias
  SelectTableRef lateral select alias ->
    P.SelectTableRef lateral <$> (fst <$> buildSelectWithParens select) <*> pure alias
  JoinTableRef joined alias ->
    P.JoinTableRef <$> buildJoinedTable joined <*> pure alias

buildTablesampleClause :: TablesampleClause -> BuildM P.TablesampleClause
buildTablesampleClause = \case
  TablesampleClause funcName exprs repeatable ->
    P.TablesampleClause
      <$> buildFuncName funcName
      <*> buildExprList exprs
      <*> traverse buildRepeatableClause repeatable

buildRepeatableClause :: RepeatableClause -> BuildM P.RepeatableClause
buildRepeatableClause = buildAExpr

buildFuncTable :: FuncTable -> BuildM P.FuncTable
buildFuncTable = \case
  FuncExprFuncTable expr ord -> P.FuncExprFuncTable <$> buildFuncExprWindowless expr <*> pure ord
  RowsFromFuncTable rows ord -> P.RowsFromFuncTable <$> buildRowsFromList rows <*> pure ord

buildFuncExprWindowless :: FuncExprWindowless -> BuildM P.FuncExprWindowless
buildFuncExprWindowless = \case
  ApplicationFuncExprWindowless application ->
    P.ApplicationFuncExprWindowless <$> buildFuncApplication application
  CommonSubexprFuncExprWindowless subexpr ->
    P.CommonSubexprFuncExprWindowless <$> buildFuncExprCommonSubexpr subexpr

buildRowsFromList :: RowsFromList -> BuildM P.RowsfromList
buildRowsFromList = traverse buildRowsFromItem

buildRowsFromItem :: RowsFromItem -> BuildM P.RowsfromItem
buildRowsFromItem = \case
  RowsFromItem expr cols -> P.RowsfromItem <$> buildFuncExprWindowless expr <*> traverse buildColDefList cols

buildColDefList :: ColDefList -> BuildM P.ColDefList
buildColDefList = buildTableFuncElementList

buildTableFuncElementList :: ColDefList -> BuildM P.TableFuncElementList
buildTableFuncElementList = traverse buildTableFuncElement

buildTableFuncElement :: TableFuncElement -> BuildM P.TableFuncElement
buildTableFuncElement = \case
  TableFuncElement col typename collate -> P.TableFuncElement col <$> buildTypename typename <*> pure collate

buildFuncAliasClause :: FuncAliasClause -> BuildM P.FuncAliasClause
buildFuncAliasClause = \case
  AliasFuncAliasClause aliasClause -> pure $ P.AliasFuncAliasClause aliasClause
  AsFuncAliasClause tableFuncElementList ->
    P.AsFuncAliasClause <$> buildTableFuncElementList tableFuncElementList
  AsColIdFuncAliasClause colId tableFuncElementList ->
    P.AsColIdFuncAliasClause colId <$> buildTableFuncElementList tableFuncElementList
  ColIdFuncAliasClause colId tableFuncElementList ->
    P.ColIdFuncAliasClause colId <$> buildTableFuncElementList tableFuncElementList

buildJoinedTable :: JoinedTable -> BuildM P.JoinedTable
buildJoinedTable = \case
  InParensJoinedTable table -> P.InParensJoinedTable <$> buildJoinedTable table
  MethJoinedTable meth lhs rhs ->
    P.MethJoinedTable
      <$> buildJoinMeth meth
      <*> buildTableRef lhs
      <*> buildTableRef rhs

buildJoinMeth :: JoinMeth -> BuildM P.JoinMeth
buildJoinMeth = \case
  CrossJoinMeth -> pure P.CrossJoinMeth
  QualJoinMeth joinType qual -> P.QualJoinMeth joinType <$> buildJoinQual qual
  NaturalJoinMeth joinType -> pure $ P.NaturalJoinMeth joinType

buildJoinQual :: JoinQual -> BuildM P.JoinQual
buildJoinQual = \case
  UsingJoinQual cols -> pure $ P.UsingJoinQual cols
  OnJoinQual expr -> P.OnJoinQual <$> buildAExpr expr

buildWhereClause :: WhereClause -> BuildM P.WhereClause
buildWhereClause = buildAExpr

buildGroupClause :: GroupClause -> BuildM P.GroupClause
buildGroupClause = traverse buildGroupByItem

buildGroupByItem :: GroupByItem -> BuildM P.GroupByItem
buildGroupByItem = \case
  ExprGroupByItem expr -> P.ExprGroupByItem <$> buildAExpr expr
  EmptyGroupingSetGroupByItem -> pure P.EmptyGroupingSetGroupByItem
  RollupGroupByItem exprs -> P.RollupGroupByItem <$> buildExprList exprs
  CubeGroupByItem exprs -> P.CubeGroupByItem <$> buildExprList exprs
  GroupingSetsGroupByItem items -> P.GroupingSetsGroupByItem <$> traverse buildGroupByItem items

buildHavingClause :: HavingClause -> BuildM P.HavingClause
buildHavingClause = buildAExpr

buildWindowClause :: WindowClause -> BuildM P.WindowClause
buildWindowClause = traverse buildWindowDefinition

buildWindowDefinition :: WindowDefinition -> BuildM P.WindowDefinition
buildWindowDefinition = \case
  WindowDefinition ident spec -> P.WindowDefinition ident <$> buildWindowSpecification spec

buildSortClause :: SortClause -> BuildM P.SortClause
buildSortClause = traverse buildSortBy

buildSortBy :: SortBy -> BuildM P.SortBy
buildSortBy = \case
  UsingSortBy expr op nullsOrder -> P.UsingSortBy <$> buildAExpr expr <*> pure op <*> pure nullsOrder
  AscDescSortBy expr ascDesc nullsOrder ->
    P.AscDescSortBy <$> buildAExpr expr <*> pure ascDesc <*> pure nullsOrder

buildSelectLimit :: SelectLimit -> BuildM P.SelectLimit
buildSelectLimit = \case
  LimitOffsetSelectLimit limit offset ->
    P.LimitOffsetSelectLimit <$> buildLimitClause limit <*> buildOffsetClause offset
  OffsetLimitSelectLimit offset limit ->
    P.OffsetLimitSelectLimit <$> buildOffsetClause offset <*> buildLimitClause limit
  LimitSelectLimit limit -> P.LimitSelectLimit <$> buildLimitClause limit
  OffsetSelectLimit offset -> P.OffsetSelectLimit <$> buildOffsetClause offset

buildLimitClause :: LimitClause -> BuildM P.LimitClause
buildLimitClause = \case
  LimitLimitClause selectLimitValue expr ->
    P.LimitLimitClause
      <$> buildSelectLimitValue selectLimitValue
      <*> traverse buildAExpr expr
  FetchOnlyLimitClause firstOrNext selectFetchFirstValue rowOrRows ->
    P.FetchOnlyLimitClause firstOrNext
      <$> traverse buildSelectFetchFirstValue selectFetchFirstValue
      <*> pure rowOrRows

buildSelectLimitValue :: SelectLimitValue -> BuildM P.SelectLimitValue
buildSelectLimitValue = \case
  ExprSelectLimitValue expr -> P.ExprSelectLimitValue <$> buildAExpr expr
  AllSelectLimitValue -> pure P.AllSelectLimitValue

buildSelectFetchFirstValue :: SelectFetchFirstValue -> BuildM P.SelectFetchFirstValue
buildSelectFetchFirstValue = \case
  ExprSelectFetchFirstValue expr -> P.ExprSelectFetchFirstValue <$> buildCExpr expr
  NumSelectFetchFirstValue plusOrMinus iOrF -> pure $ P.NumSelectFetchFirstValue plusOrMinus iOrF

buildOffsetClause :: OffsetClause -> BuildM P.OffsetClause
buildOffsetClause = \case
  ExprOffsetClause expr -> P.ExprOffsetClause <$> buildAExpr expr
  FetchFirstOffsetClause selectFetchFirstValue rowOrRows ->
    P.FetchFirstOffsetClause
      <$> buildSelectFetchFirstValue selectFetchFirstValue
      <*> pure rowOrRows

buildForLockingClause :: ForLockingClause -> BuildM P.ForLockingClause
buildForLockingClause = \case
  ItemsForLockingClause items -> P.ItemsForLockingClause <$> traverse buildForLockingItem items
  ReadOnlyForLockingClause -> pure P.ReadOnlyForLockingClause

buildForLockingItem :: ForLockingItem -> BuildM P.ForLockingItem
buildForLockingItem = \case
  ForLockingItem strength names nowaitOrSkip ->
    P.ForLockingItem strength <$> (traverse . traverse) buildQualifiedName names <*> pure nowaitOrSkip

buildInsertTarget :: InsertTarget -> BuildM P.InsertTarget
buildInsertTarget = \case
  InsertTarget name alias -> P.InsertTarget <$> buildQualifiedName name <*> pure alias

buildInsertRest :: InsertRest -> BuildM P.InsertRest
buildInsertRest = \case
  SelectInsertRest columns override select ->
    P.SelectInsertRest
      <$> traverse buildInsertColumnList columns
      <*> pure override
      <*> (fst <$> buildSelectStmt select)
  DefaultValuesInsertRest -> pure P.DefaultValuesInsertRest

buildInsertColumnList :: InsertColumnList -> BuildM P.InsertColumnList
buildInsertColumnList = traverse buildInsertColumnItem

buildInsertColumnItem :: InsertColumnItem -> BuildM P.InsertColumnItem
buildInsertColumnItem = \case
  InsertColumnItem col indirection -> P.InsertColumnItem col <$> traverse buildIndirection indirection

buildOnConflict :: OnConflict -> BuildM P.OnConflict
buildOnConflict = \case
  OnConflict confExpr doExpr -> P.OnConflict <$> traverse buildConfExpr confExpr <*> buildOnConflictDo doExpr

buildConfExpr :: ConfExpr -> BuildM P.ConfExpr
buildConfExpr = \case
  WhereConfExpr params whereClause ->
    P.WhereConfExpr
      <$> buildIndexParams params
      <*> traverse buildWhereClause whereClause
  ConstraintConfExpr name -> pure $ P.ConstraintConfExpr name

buildIndexParams :: IndexParams -> BuildM P.IndexParams
buildIndexParams = traverse buildIndexElem

buildIndexElem :: IndexElem -> BuildM P.IndexElem
buildIndexElem = \case
  IndexElem elemDef collate class_ ascDesc nullsOrder -> do
    elemDef' <- buildIndexElemDef elemDef
    pure $ P.IndexElem elemDef' collate class_ ascDesc nullsOrder

buildIndexElemDef :: IndexElemDef -> BuildM P.IndexElemDef
buildIndexElemDef = \case
  IdIndexElemDef col -> pure $ P.IdIndexElemDef col
  FuncIndexElemDef expr -> P.FuncIndexElemDef <$> buildFuncExprWindowless expr
  ExprIndexElemDef expr -> P.ExprIndexElemDef <$> buildAExpr expr

buildOnConflictDo :: OnConflictDo -> BuildM P.OnConflictDo
buildOnConflictDo = \case
  UpdateOnConflictDo setClauses whereClause ->
    P.UpdateOnConflictDo <$> buildSetClauseList setClauses <*> traverse buildWhereClause whereClause
  NothingOnConflictDo -> pure P.NothingOnConflictDo

buildSetClauseList :: SetClauseList -> BuildM P.SetClauseList
buildSetClauseList = traverse buildSetClause

buildSetClause :: SetClause -> BuildM P.SetClause
buildSetClause = \case
  TargetSetClause target expr -> P.TargetSetClause <$> buildSetTarget target <*> buildAExpr expr
  TargetListSetClause targets expr -> P.TargetListSetClause <$> buildSetTargetList targets <*> buildAExpr expr

buildSetTarget :: SetTarget -> BuildM P.SetTarget
buildSetTarget = \case
  SetTarget col indirection -> P.SetTarget col <$> traverse buildIndirection indirection

buildSetTargetList :: SetTargetList -> BuildM P.SetTargetList
buildSetTargetList = traverse buildSetTarget

buildReturningClause :: ReturningClause row -> BuildM (Maybe P.ReturningClause, RowDecoders row)
buildReturningClause = \case
  EmptyReturningClause -> pure (Nothing, DecodersNil)
  TargetListReturningClause targets -> first Just <$> buildTargetList targets

buildRelationExprOptAlias :: RelationExprOptAlias -> BuildM P.RelationExprOptAlias
buildRelationExprOptAlias = \case
  RelationExprOptAlias expr alias -> flip P.RelationExprOptAlias alias <$> buildRelationExpr expr

buildWhereOrCurrentClause :: WhereOrCurrentClause -> BuildM P.WhereOrCurrentClause
buildWhereOrCurrentClause = \case
  ExprWhereOrCurrentClause expr -> P.ExprWhereOrCurrentClause <$> buildAExpr expr
  CursorWhereOrCurrentClause name -> pure $ P.CursorWhereOrCurrentClause name

buildUsingClause :: UsingClause -> BuildM P.UsingClause
buildUsingClause = buildFromList

buildFromList :: FromList -> BuildM P.FromList
buildFromList = traverse buildTableRef

data RowDecoders row where
  DecodersNil :: RowDecoders '[]
  DecodersCons :: Decoders.Row (ColumnToHask t) -> RowDecoders row -> RowDecoders (t ': row)

appendRowDecoders :: RowDecoders row -> RowDecoders row' -> RowDecoders (row ++ row')
appendRowDecoders DecodersNil row' = row'
appendRowDecoders (DecodersCons a row) row' = DecodersCons a (appendRowDecoders row row')

data RowResult row where
  ResultNil :: RowResult '[]
  (:::) :: ColumnToHask t -> RowResult row -> RowResult (t ': row)

compileDecoders :: RowDecoders row -> Decoders.Row (RowResult row)
compileDecoders DecodersNil = pure ResultNil
compileDecoders (DecodersCons row decoders) = (:::) <$> row <*> compileDecoders decoders
