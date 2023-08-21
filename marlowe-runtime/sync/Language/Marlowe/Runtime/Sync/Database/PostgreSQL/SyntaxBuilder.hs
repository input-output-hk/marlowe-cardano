{-# OPTIONS_GHC -Wno-orphans #-}

-- | A tool for building dynamic parameterized SQL using an abstract syntax tree. Conceptually similar to
-- hasql-dynamic-statements, except it uses postgresql-syntax to build the statements in a structured way instead of raw
-- ByteString SQL snippets, which are difficult to compose and error-prone.
--
-- The API is derived by creating lifted versions of the data constructors in PostgresqlSyntax.Ast so that regular
-- function application can be used rather than having to use <$> and <*> everywhere. The API is not complete, please
-- add missing constructors to the API as they are needed.
module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.SyntaxBuilder where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Trans.RWS (RWS, runRWS, rws)
import Data.Bitraversable (bisequence)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Contravariant ((>$), (>$<))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import Data.String (IsString)
import GHC.Exts (IsString (..))
import Hasql.Decoders (Result)
import Hasql.Encoders (NullableOrNot, Params, Value, foldableArray)
import qualified Hasql.Encoders as Encoders
import Hasql.Implicits.Encoders (DefaultParamEncoder, defaultParam)
import Hasql.Statement (Statement (..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import PostgresqlSyntax.Ast
import PostgresqlSyntax.Rendering (toByteString)
import qualified PostgresqlSyntax.Rendering as Rendering

newtype SyntaxBuilder a = SyntaxBuilder
  { runSyntaxBuilder :: RWS () (Params ()) Int a
  }
  deriving newtype (Functor, Applicative, Monad)

buildStatement :: SyntaxBuilder PreparableStmt -> Result result -> Statement () result
buildStatement builder result = Statement (toByteString $ Rendering.preparableStmt stmt) encoder result $ paramCount > 1
  where
    (stmt, paramCount, encoder) = runRWS (runSyntaxBuilder builder) () 1

-- PreparableStmt helpers

selectPreparableStmt :: SyntaxBuilder SelectStmt -> SyntaxBuilder PreparableStmt
selectPreparableStmt = fmap SelectPreparableStmt

insertPreparableStmt :: SyntaxBuilder InsertStmt -> SyntaxBuilder PreparableStmt
insertPreparableStmt = fmap InsertPreparableStmt

updatePreparableStmt :: SyntaxBuilder UpdateStmt -> SyntaxBuilder PreparableStmt
updatePreparableStmt = fmap UpdatePreparableStmt

deletePreparableStmt :: SyntaxBuilder DeleteStmt -> SyntaxBuilder PreparableStmt
deletePreparableStmt = fmap DeletePreparableStmt

callPreparableStmt :: SyntaxBuilder CallStmt -> SyntaxBuilder PreparableStmt
callPreparableStmt = fmap CallPreparableStmt

-- SelectStmt helpers

selectStmtNoParens :: SyntaxBuilder SelectNoParens -> SyntaxBuilder SelectStmt
selectStmtNoParens = fmap Left

selectStmtWithParens :: SyntaxBuilder SelectWithParens -> SyntaxBuilder SelectStmt
selectStmtWithParens = fmap Right

-- SelectNoParens helpers

selectNoParens
  :: Maybe (SyntaxBuilder WithClause)
  -> SyntaxBuilder SelectClause
  -> Maybe (SyntaxBuilder SortClause)
  -> Maybe (SyntaxBuilder SelectLimit)
  -> Maybe (SyntaxBuilder ForLockingClause)
  -> SyntaxBuilder SelectNoParens
selectNoParens with select sort limit locking =
  SelectNoParens <$> sequence with <*> select <*> sequence sort <*> sequence limit <*> sequence locking

-- WithClause helpers

withClause :: Bool -> NonEmpty (SyntaxBuilder CommonTableExpr) -> SyntaxBuilder WithClause
withClause recursive tables = WithClause recursive <$> sequence tables

-- CommonTableExpr helpers

commonTableExpr
  :: Ident -> Maybe (NonEmpty Ident) -> Maybe Bool -> SyntaxBuilder PreparableStmt -> SyntaxBuilder CommonTableExpr
commonTableExpr name columns optMaterialized stmt = CommonTableExpr name columns optMaterialized <$> stmt

-- SelectClause helpers

simpleSelect :: SyntaxBuilder SimpleSelect -> SyntaxBuilder SelectClause
simpleSelect = fmap Left

selectWithParens :: SyntaxBuilder SelectWithParens -> SyntaxBuilder SelectClause
selectWithParens = fmap Right

-- SimpleSelect helpers

normalSimpleSelect
  :: Maybe (SyntaxBuilder Targeting)
  -> Maybe (SyntaxBuilder IntoClause)
  -> Maybe (NonEmpty (SyntaxBuilder TableRef))
  -> Maybe (SyntaxBuilder WhereClause)
  -> Maybe (SyntaxBuilder GroupClause)
  -> Maybe (SyntaxBuilder HavingClause)
  -> Maybe (SyntaxBuilder WindowClause)
  -> SyntaxBuilder SimpleSelect
normalSimpleSelect targeting into from where_ group having window =
  NormalSimpleSelect
    <$> sequence targeting
    <*> sequence into
    <*> (getCompose <$> sequence (Compose from))
    <*> sequence where_
    <*> sequence group
    <*> sequence having
    <*> sequence window

valuesSimpleSelect :: SyntaxBuilder ValuesClause -> SyntaxBuilder SimpleSelect
valuesSimpleSelect = fmap ValuesSimpleSelect

tableSimpleSelect :: SyntaxBuilder RelationExpr -> SyntaxBuilder SimpleSelect
tableSimpleSelect = fmap TableSimpleSelect

binSimpleSelect
  :: SelectBinOp -> SyntaxBuilder SelectClause -> Maybe Bool -> SyntaxBuilder SelectClause -> SyntaxBuilder SimpleSelect
binSimpleSelect op lhs optAllOrDistinct rhs = BinSimpleSelect op <$> lhs <*> pure optAllOrDistinct <*> rhs

-- Targeting helpers

normalTargeting :: NonEmpty (SyntaxBuilder TargetEl) -> SyntaxBuilder Targeting
normalTargeting = fmap NormalTargeting . sequence

allTargeting :: Maybe (NonEmpty (SyntaxBuilder TargetEl)) -> SyntaxBuilder Targeting
allTargeting = fmap (AllTargeting . getCompose) . sequence . Compose

distinctTargeting
  :: Maybe (NonEmpty (SyntaxBuilder AExpr)) -> NonEmpty (SyntaxBuilder TargetEl) -> SyntaxBuilder Targeting
distinctTargeting exprList targetList =
  DistinctTargeting . getCompose <$> sequence (Compose exprList) <*> sequence targetList

-- TargetEl helpers

aliasedExprTargetEl :: SyntaxBuilder AExpr -> Ident -> SyntaxBuilder TargetEl
aliasedExprTargetEl target alias = flip AliasedExprTargetEl alias <$> target

implicitlyAliasedExprTargetEl :: SyntaxBuilder AExpr -> Ident -> SyntaxBuilder TargetEl
implicitlyAliasedExprTargetEl target alias = flip ImplicitlyAliasedExprTargetEl alias <$> target

exprTargetEl :: SyntaxBuilder AExpr -> SyntaxBuilder TargetEl
exprTargetEl = fmap ExprTargetEl

asteriskTargetEl :: SyntaxBuilder TargetEl
asteriskTargetEl = pure AsteriskTargetEl

column :: Ident -> Ident -> SyntaxBuilder TargetEl
column relName colName =
  exprTargetEl $
    cExprAExpr $
      columnrefCExpr $
        Columnref relName $
          Just $
            pure $
              AttrNameIndirectionEl colName

-- AExpr helpers

cExprAExpr :: SyntaxBuilder CExpr -> SyntaxBuilder AExpr
cExprAExpr = fmap CExprAExpr

typecastAExpr :: SyntaxBuilder AExpr -> Typename -> SyntaxBuilder AExpr
typecastAExpr expr typename = flip TypecastAExpr typename <$> expr

collateAExpr :: SyntaxBuilder AExpr -> AnyName -> SyntaxBuilder AExpr
collateAExpr expr name = flip CollateAExpr name <$> expr

atTimeZoneAExpr :: SyntaxBuilder AExpr -> SyntaxBuilder AExpr -> SyntaxBuilder AExpr
atTimeZoneAExpr = liftA2 AtTimeZoneAExpr

plusAExpr :: SyntaxBuilder AExpr -> SyntaxBuilder AExpr
plusAExpr = fmap PlusAExpr

minusAExpr :: SyntaxBuilder AExpr -> SyntaxBuilder AExpr
minusAExpr = fmap MinusAExpr

symbolicBinOpAExpr :: SyntaxBuilder AExpr -> SymbolicExprBinOp -> SyntaxBuilder AExpr -> SyntaxBuilder AExpr
symbolicBinOpAExpr lhs op rhs = SymbolicBinOpAExpr <$> lhs <*> pure op <*> rhs

equalsAExpr :: SyntaxBuilder AExpr -> SyntaxBuilder AExpr -> SyntaxBuilder AExpr
equalsAExpr lhs = symbolicBinOpAExpr lhs $ MathSymbolicExprBinOp EqualsMathOp

prefixQualOpAExpr :: QualOp -> SyntaxBuilder AExpr -> SyntaxBuilder AExpr
prefixQualOpAExpr = fmap . PrefixQualOpAExpr

suffixQualOpAExpr :: SyntaxBuilder AExpr -> QualOp -> SyntaxBuilder AExpr
suffixQualOpAExpr expr op = flip SuffixQualOpAExpr op <$> expr

andAExpr :: SyntaxBuilder AExpr -> SyntaxBuilder AExpr -> SyntaxBuilder AExpr
andAExpr = liftA2 AndAExpr

orAExpr :: SyntaxBuilder AExpr -> SyntaxBuilder AExpr -> SyntaxBuilder AExpr
orAExpr = liftA2 OrAExpr

notAExpr :: SyntaxBuilder AExpr -> SyntaxBuilder AExpr
notAExpr = fmap NotAExpr

verbalExprBinOpAExpr
  :: SyntaxBuilder AExpr
  -> Bool
  -> VerbalExprBinOp
  -> SyntaxBuilder AExpr
  -> Maybe (SyntaxBuilder AExpr)
  -> SyntaxBuilder AExpr
verbalExprBinOpAExpr lhs invert op rhs escape =
  VerbalExprBinOpAExpr
    <$> lhs
    <*> pure invert
    <*> pure op
    <*> rhs
    <*> sequence escape

reversableOpAExpr :: SyntaxBuilder AExpr -> Bool -> AExprReversableOp -> SyntaxBuilder AExpr
reversableOpAExpr expr invert op = ReversableOpAExpr <$> expr <*> pure invert <*> pure op

isnullAExpr :: SyntaxBuilder AExpr -> SyntaxBuilder AExpr
isnullAExpr = fmap IsnullAExpr

notnullAExpr :: SyntaxBuilder AExpr -> SyntaxBuilder AExpr
notnullAExpr = fmap NotnullAExpr

overlapsAExpr :: SyntaxBuilder Row -> SyntaxBuilder Row -> SyntaxBuilder AExpr
overlapsAExpr = liftA2 OverlapsAExpr

subqueryAExpr
  :: SyntaxBuilder AExpr
  -> SubqueryOp
  -> SubType
  -> Either (SyntaxBuilder SelectWithParens) (SyntaxBuilder AExpr)
  -> SyntaxBuilder AExpr
subqueryAExpr expr op subType sub =
  SubqueryAExpr
    <$> expr
    <*> pure op
    <*> pure subType
    <*> bisequence sub

uniqueAExpr :: SyntaxBuilder SelectWithParens -> SyntaxBuilder AExpr
uniqueAExpr = fmap UniqueAExpr

defaultAExpr :: SyntaxBuilder AExpr
defaultAExpr = pure DefaultAExpr

-- CExpr helpers

columnrefCExpr :: Columnref -> SyntaxBuilder CExpr
columnrefCExpr = pure . ColumnrefCExpr

aexprConstCExpr :: AexprConst -> SyntaxBuilder CExpr
aexprConstCExpr = pure . AexprConstCExpr

paramCExpr :: (DefaultParamEncoder param) => param -> SyntaxBuilder CExpr
paramCExpr = encoderAndParamCExpr defaultParam

encoderAndParamCExpr :: NullableOrNot Value param -> param -> SyntaxBuilder CExpr
encoderAndParamCExpr encoder param = SyntaxBuilder $ rws \_ paramId ->
  ( ParamCExpr paramId Nothing
  , succ paramId
  , param >$ Encoders.param encoder
  )

inParensCExpr :: SyntaxBuilder AExpr -> Maybe Indirection -> SyntaxBuilder CExpr
inParensCExpr expr indirection = flip InParensCExpr indirection <$> expr

caseCExpr :: SyntaxBuilder CaseExpr -> SyntaxBuilder CExpr
caseCExpr = fmap CaseCExpr

funcCExpr :: SyntaxBuilder FuncExpr -> SyntaxBuilder CExpr
funcCExpr = fmap FuncCExpr

selectWithParensCExpr :: SyntaxBuilder SelectWithParens -> Maybe Indirection -> SyntaxBuilder CExpr
selectWithParensCExpr select indirection = flip SelectWithParensCExpr indirection <$> select

existsCExpr :: SyntaxBuilder SelectWithParens -> SyntaxBuilder CExpr
existsCExpr = fmap ExistsCExpr

arrayCExpr :: Either (SyntaxBuilder SelectWithParens) (SyntaxBuilder ArrayExpr) -> SyntaxBuilder CExpr
arrayCExpr = fmap ArrayCExpr . bisequence

explicitRowCExpr :: SyntaxBuilder ExplicitRow -> SyntaxBuilder CExpr
explicitRowCExpr = fmap ExplicitRowCExpr

implicitRowCExpr :: SyntaxBuilder ImplicitRow -> SyntaxBuilder CExpr
implicitRowCExpr = fmap ImplicitRowCExpr

groupingCExpr :: NonEmpty (SyntaxBuilder AExpr) -> SyntaxBuilder CExpr
groupingCExpr = fmap GroupingCExpr . sequence

-- TableRef helpers

relationExprTableRef :: RelationExpr -> Maybe AliasClause -> Maybe TablesampleClause -> SyntaxBuilder TableRef
relationExprTableRef rel alias tablesample = pure $ RelationExprTableRef rel alias tablesample

funcTableRef
  :: Bool -> SyntaxBuilder FuncTable -> Maybe FuncAliasClause -> SyntaxBuilder TableRef
funcTableRef lateral func alias = FuncTableRef lateral <$> func <*> pure alias

selectTableRef :: Bool -> SyntaxBuilder SelectWithParens -> Maybe AliasClause -> SyntaxBuilder TableRef
selectTableRef lateral select alias = SelectTableRef lateral <$> select <*> pure alias

joinTableRef :: SyntaxBuilder JoinedTable -> Maybe AliasClause -> SyntaxBuilder TableRef
joinTableRef joined alias = JoinTableRef <$> joined <*> pure alias

table :: Maybe Ident -> Ident -> SyntaxBuilder TableRef
table schema tab =
  relationExprTableRef
    ( SimpleRelationExpr
        ( case schema of
            Nothing -> SimpleQualifiedName tab
            Just s -> IndirectedQualifiedName s $ pure $ AttrNameIndirectionEl tab
        )
        False
    )
    Nothing
    Nothing

-- FuncTable helpers

funcExprFuncTable :: SyntaxBuilder FuncExprWindowless -> OptOrdinality -> SyntaxBuilder FuncTable
funcExprFuncTable expr ord = flip FuncExprFuncTable ord <$> expr

rowsFromFuncTable :: NonEmpty (SyntaxBuilder RowsfromItem) -> OptOrdinality -> SyntaxBuilder FuncTable
rowsFromFuncTable rows ord = flip RowsFromFuncTable ord <$> sequence rows

-- FuncExprWindowless helpers

applicationFuncExprWindowless :: SyntaxBuilder FuncApplication -> SyntaxBuilder FuncExprWindowless
applicationFuncExprWindowless = fmap ApplicationFuncExprWindowless

commonSubexprFuncExprWindowless :: SyntaxBuilder FuncExprCommonSubexpr -> SyntaxBuilder FuncExprWindowless
commonSubexprFuncExprWindowless = fmap CommonSubexprFuncExprWindowless

-- FuncApplication helpers

funcApplication :: FuncName -> Maybe (SyntaxBuilder FuncApplicationParams) -> SyntaxBuilder FuncApplication
funcApplication name = fmap (FuncApplication name) . sequence

simpleFuncApplication :: FuncName -> [SyntaxBuilder FuncArgExpr] -> SyntaxBuilder FuncApplication
simpleFuncApplication name [] = funcApplication name Nothing
simpleFuncApplication name args =
  funcApplication name $ Just $ normalFuncApplicationParams Nothing (NE.fromList args) Nothing

-- FuncApplicationParams helpers

normalFuncApplicationParams
  :: Maybe Bool
  -> NonEmpty (SyntaxBuilder FuncArgExpr)
  -> Maybe (SyntaxBuilder SortClause)
  -> SyntaxBuilder FuncApplicationParams
normalFuncApplicationParams optAllOrDistinct args sortClause =
  NormalFuncApplicationParams optAllOrDistinct <$> sequence args <*> sequence sortClause

variadicFuncApplicationParams
  :: Maybe (NonEmpty (SyntaxBuilder FuncArgExpr))
  -> SyntaxBuilder FuncArgExpr
  -> Maybe (SyntaxBuilder SortClause)
  -> SyntaxBuilder FuncApplicationParams
variadicFuncApplicationParams args arg sortClause =
  VariadicFuncApplicationParams <$> (getCompose <$> sequence (Compose args)) <*> arg <*> sequence sortClause

starFuncApplicationParams :: SyntaxBuilder FuncApplicationParams
starFuncApplicationParams = pure StarFuncApplicationParams

-- FuncArgExpr helpers

exprFuncArgExpr :: SyntaxBuilder AExpr -> SyntaxBuilder FuncArgExpr
exprFuncArgExpr = fmap ExprFuncArgExpr

colonEqualsFuncArgExpr :: Ident -> SyntaxBuilder AExpr -> SyntaxBuilder FuncArgExpr
colonEqualsFuncArgExpr = fmap . ColonEqualsFuncArgExpr

equalsGreaterFuncArgExpr :: Ident -> SyntaxBuilder AExpr -> SyntaxBuilder FuncArgExpr
equalsGreaterFuncArgExpr = fmap . EqualsGreaterFuncArgExpr

-- JoinedTable helpers

inParensJoinedTable :: SyntaxBuilder JoinedTable -> SyntaxBuilder JoinedTable
inParensJoinedTable = fmap InParensJoinedTable

methJoinedTable
  :: SyntaxBuilder JoinMeth -> SyntaxBuilder TableRef -> SyntaxBuilder TableRef -> SyntaxBuilder JoinedTable
methJoinedTable = liftA3 MethJoinedTable

naturalJoin :: Maybe JoinType -> SyntaxBuilder TableRef -> SyntaxBuilder TableRef -> SyntaxBuilder JoinedTable
naturalJoin = methJoinedTable . naturalJoinMeth

-- JoinMeth helpers

crossJoinMeth :: SyntaxBuilder JoinMeth
crossJoinMeth = pure CrossJoinMeth

qualJoinMeth :: Maybe JoinType -> SyntaxBuilder JoinQual -> SyntaxBuilder JoinMeth
qualJoinMeth = fmap . QualJoinMeth

naturalJoinMeth :: Maybe JoinType -> SyntaxBuilder JoinMeth
naturalJoinMeth = pure . NaturalJoinMeth

-- JoinQual helpers

usingJoinQual :: NonEmpty Ident -> SyntaxBuilder JoinQual
usingJoinQual = pure . UsingJoinQual

onJoinQual :: SyntaxBuilder AExpr -> SyntaxBuilder JoinQual
onJoinQual = fmap OnJoinQual

instance IsString Ident where
  fromString = UnquotedIdent . fromString

instance IsString FuncName where
  fromString = TypeFuncName . fromString

instance (DefaultParamEncoder a) => DefaultParamEncoder (Set a) where
  defaultParam = Encoders.nonNullable $ foldableArray defaultParam

instance DefaultParamEncoder Chain.TxId where
  defaultParam = Encoders.nonNullable $ Chain.unTxId >$< Encoders.bytea

instance DefaultParamEncoder Chain.TxIx where
  defaultParam = Encoders.nonNullable $ fromIntegral >$< Encoders.int2

instance DefaultParamEncoder Chain.PolicyId where
  defaultParam = Encoders.nonNullable $ Chain.unPolicyId >$< Encoders.bytea

instance DefaultParamEncoder Chain.TokenName where
  defaultParam = Encoders.nonNullable $ Chain.unTokenName >$< Encoders.bytea
