{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This is a modified version of PostgresqlSyntax.Ast which carries values to apply to parameters and decoders for
-- results.
module Hasql.DynamicSyntax.Ast where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import qualified Hasql.Encoders as Encoders
import Network.IP.Addr (IP, NetAddr)
import PostgresqlSyntax.Ast (
  AliasClause,
  AnyName,
  AscDesc,
  Attrs,
  Bconst,
  Character,
  Class,
  ColId,
  Collate,
  CollateClause,
  ConstCharacter,
  ConstDatetime,
  CursorName,
  ExistingWindowName,
  ExtractArg,
  Fconst,
  ForLockingStrength,
  FrameClauseMode,
  Iconst,
  Ident,
  Interval,
  JoinType,
  Name,
  NullsOrder,
  OptOrdinality,
  OptVarying,
  OverrideKind,
  QualAllOp,
  QualOp,
  Sconst,
  SelectBinOp,
  SubType,
  SubqueryOp,
  SymbolicExprBinOp,
  TrimModifier,
  TypeFunctionName,
  TypenameArrayDimensions,
  VerbalExprBinOp,
  WindowExclusionClause,
  Xconst,
 )

data Null
data NotNull

data Nullability a where
  Null :: Nullability Null
  NotNull :: Nullability NotNull

deriving instance Show (Nullability a)
deriving instance Eq (Nullability a)
deriving instance Ord (Nullability a)

data SqlBool
data SqlInt2
data SqlInt4
data SqlInt8
data SqlFloat4
data SqlFloat8
data SqlNumeric
data SqlChar
data SqlText
data SqlBytea
data SqlDate
data SqlTimestamp
data SqlTimestampz
data SqlTime
data SqlTimez
data SqlInterval
data SqlUUID
data SqlInet
data SqlJson
data SqlJsonb
data SqlArray t

data SqlType t where
  SqlBool :: SqlType SqlBool
  SqlInt2 :: SqlType SqlInt2
  SqlInt4 :: SqlType SqlInt4
  SqlInt8 :: SqlType SqlInt8
  SqlFloat4 :: SqlType SqlFloat4
  SqlFloat8 :: SqlType SqlFloat8
  SqlNumeric :: SqlType SqlNumeric
  SqlChar :: SqlType SqlChar
  SqlText :: SqlType SqlText
  SqlBytea :: SqlType SqlBytea
  SqlDate :: SqlType SqlDate
  SqlTimestamp :: SqlType SqlTimestamp
  SqlTimestampz :: SqlType SqlTimestampz
  SqlTime :: SqlType SqlTime
  SqlTimez :: SqlType SqlTimez
  SqlInterval :: SqlType SqlInterval
  SqlUUID :: SqlType SqlUUID
  SqlInet :: SqlType SqlInet
  SqlJson :: SqlType SqlJson
  SqlJsonb :: SqlType SqlJsonb
  SqlArray :: !(ColumnType t) -> SqlType (SqlArray (ColumnType t))

deriving instance Show (SqlType t)
deriving instance Eq (SqlType t)
deriving instance Ord (SqlType t)

data ColumnType t where
  ColumnType :: !(SqlType t) -> !(Nullability nullable) -> ColumnType '(t, nullable)

deriving instance Show (ColumnType t)
deriving instance Eq (ColumnType t)
deriving instance Ord (ColumnType t)

data DeclareRow row where
  DeclareNil :: DeclareRow '[]
  DeclareCons :: ColumnType t -> DeclareRow row -> DeclareRow (t ': row)

deriving instance Show (DeclareRow t)
deriving instance Eq (DeclareRow t)
deriving instance Ord (DeclareRow t)

type family SqlToHask (t :: Type) :: Type where
  SqlToHask SqlBool = Bool
  SqlToHask SqlInt2 = Int16
  SqlToHask SqlInt4 = Int32
  SqlToHask SqlInt8 = Int64
  SqlToHask SqlFloat4 = Float
  SqlToHask SqlFloat8 = Double
  SqlToHask SqlNumeric = Scientific
  SqlToHask SqlChar = Char
  SqlToHask SqlText = Text
  SqlToHask SqlBytea = ByteString
  SqlToHask SqlDate = Day
  SqlToHask SqlTimestamp = LocalTime
  SqlToHask SqlTimestampz = UTCTime
  SqlToHask SqlTime = TimeOfDay
  SqlToHask SqlTimez = (TimeOfDay, TimeZone)
  SqlToHask SqlInterval = DiffTime
  SqlToHask SqlUUID = UUID
  SqlToHask SqlInet = NetAddr IP
  SqlToHask SqlJson = Value
  SqlToHask SqlJsonb = Value
  SqlToHask (SqlArray (ColumnType t)) = Vector (ColumnToHask t)

type family ColumnToHask (t :: (Type, Type)) :: Type where
  ColumnToHask '(t, Null) = Maybe (SqlToHask t)
  ColumnToHask '(t, NotNull) = (SqlToHask t)

type family (++) (as :: [k]) (as' :: [k]) :: [k] where
  '[] ++ as' = as'
  (a ': as) ++ as' = a ': (as ++ as')

-- * Statement

-- |
-- ==== References
-- @
-- PreparableStmt:
--   |  SelectStmt
--   |  InsertStmt
--   |  UpdateStmt
--   |  DeleteStmt
--   |  CallStmt
-- @
data PreparableStmt row
  = SelectPreparableStmt (SelectStmt row)
  | InsertPreparableStmt (InsertStmt row)
  | UpdatePreparableStmt (UpdateStmt row)
  | DeletePreparableStmt (DeleteStmt row)
  | CallPreparableStmt (CallStmt row)

-- * Call

data CallStmt row
  = CallStmt (DeclareRow row) FuncApplication

-- * Insert

-- |
-- ==== References
-- @
-- InsertStmt:
--   | opt_with_clause INSERT INTO insert_target insert_rest
--       opt_on_conflict returning_clause
-- @
data InsertStmt row = InsertStmt (Maybe WithClause) InsertTarget InsertRest (Maybe OnConflict) (ReturningClause row)

-- |
-- ==== References
-- @
-- insert_target:
--   | qualified_name
--   | qualified_name AS ColId
-- @
data InsertTarget = InsertTarget QualifiedName (Maybe ColId)

-- |
-- ==== References
-- @
-- insert_rest:
--   | SelectStmt
--   | OVERRIDING override_kind VALUE_P SelectStmt
--   | '(' insert_column_list ')' SelectStmt
--   | '(' insert_column_list ')' OVERRIDING override_kind VALUE_P SelectStmt
--   | DEFAULT VALUES
-- @
data InsertRest
  = forall row. SelectInsertRest (Maybe InsertColumnList) (Maybe OverrideKind) (SelectStmt row)
  | DefaultValuesInsertRest

-- |
-- ==== References
-- @
-- insert_column_list:
--   | insert_column_item
--   | insert_column_list ',' insert_column_item
-- @
type InsertColumnList = NonEmpty InsertColumnItem

-- |
-- ==== References
-- @
-- insert_column_item:
--   | ColId opt_indirection
-- @
data InsertColumnItem = InsertColumnItem ColId (Maybe Indirection)

-- |
-- ==== References
-- @
-- opt_on_conflict:
--   | ON CONFLICT opt_conf_expr DO UPDATE SET set_clause_list where_clause
--   | ON CONFLICT opt_conf_expr DO NOTHING
--   | EMPTY
-- @
data OnConflict = OnConflict (Maybe ConfExpr) OnConflictDo

-- |
-- ==== References
-- @
-- opt_on_conflict:
--   | ON CONFLICT opt_conf_expr DO UPDATE SET set_clause_list where_clause
--   | ON CONFLICT opt_conf_expr DO NOTHING
--   | EMPTY
-- @
data OnConflictDo
  = UpdateOnConflictDo SetClauseList (Maybe WhereClause)
  | NothingOnConflictDo

-- |
-- ==== References
-- @
-- opt_conf_expr:
--   | '(' index_params ')' where_clause
--   | ON CONSTRAINT name
--   | EMPTY
-- @
data ConfExpr
  = WhereConfExpr IndexParams (Maybe WhereClause)
  | ConstraintConfExpr Name

-- |
-- ==== References
-- @
-- returning_clause:
--   | RETURNING target_list
--   | EMPTY
-- @
data ReturningClause row where
  EmptyReturningClause :: ReturningClause '[]
  TargetListReturningClause :: TargetList row -> ReturningClause row

-- * Update

-- |
-- ==== References
-- @
-- UpdateStmt:
--   | opt_with_clause UPDATE relation_expr_opt_alias
--       SET set_clause_list
--       from_clause
--       where_or_current_clause
--       returning_clause
-- @
data UpdateStmt row
  = UpdateStmt
      (Maybe WithClause)
      RelationExprOptAlias
      SetClauseList
      (Maybe FromClause)
      (Maybe WhereOrCurrentClause)
      (ReturningClause row)

-- |
-- ==== References
-- @
-- set_clause_list:
--   | set_clause
--   | set_clause_list ',' set_clause
-- @
type SetClauseList = NonEmpty SetClause

-- |
-- ==== References
-- @
-- set_clause:
--   | set_target '=' a_expr
--   | '(' set_target_list ')' '=' a_expr
-- @
data SetClause
  = TargetSetClause SetTarget AExpr
  | TargetListSetClause SetTargetList AExpr

-- |
-- ==== References
-- @
-- set_target:
--   | ColId opt_indirection
-- @
data SetTarget = SetTarget ColId (Maybe Indirection)

-- |
-- ==== References
-- @
-- set_target_list:
--   | set_target
--   | set_target_list ',' set_target
-- @
type SetTargetList = NonEmpty SetTarget

-- * Delete

-- |
-- ==== References
-- @
-- DeleteStmt:
--   | opt_with_clause DELETE_P FROM relation_expr_opt_alias
--       using_clause where_or_current_clause returning_clause
-- @
data DeleteStmt row
  = DeleteStmt
      (Maybe WithClause)
      RelationExprOptAlias
      (Maybe UsingClause)
      (Maybe WhereOrCurrentClause)
      (ReturningClause row)

-- |
-- ==== References
-- @
-- using_clause:
--   | USING from_list
--   | EMPTY
-- @
type UsingClause = FromList

-- * Select

-- |
-- ==== References
-- @
-- SelectStmt:
--   |  select_no_parens
--   |  select_with_parens
-- @
type SelectStmt row = Either (SelectNoParens row) (SelectWithParens row)

-- |
-- ==== References
-- @
-- select_with_parens:
--   |  '(' select_no_parens ')'
--   |  '(' select_with_parens ')'
-- @
data SelectWithParens row
  = NoParensSelectWithParens (SelectNoParens row)
  | WithParensSelectWithParens (SelectWithParens row)

-- |
-- Covers the following cases:
--
-- @
-- select_no_parens:
--   |  simple_select
--   |  select_clause sort_clause
--   |  select_clause opt_sort_clause for_locking_clause opt_select_limit
--   |  select_clause opt_sort_clause select_limit opt_for_locking_clause
--   |  with_clause select_clause
--   |  with_clause select_clause sort_clause
--   |  with_clause select_clause opt_sort_clause for_locking_clause opt_select_limit
--   |  with_clause select_clause opt_sort_clause select_limit opt_for_locking_clause
-- @
data SelectNoParens row
  = SelectNoParens (Maybe WithClause) (SelectClause row) (Maybe SortClause) (Maybe SelectLimit) (Maybe ForLockingClause)

-- |
-- @
-- select_clause:
--   |  simple_select
--   |  select_with_parens
-- @
type SelectClause row = Either (SimpleSelect row) (SelectWithParens row)

-- |
-- ==== References
-- @
-- simple_select:
--   |  SELECT opt_all_clause opt_target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--   |  SELECT distinct_clause target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--   |  values_clause
--   |  TABLE relation_expr
--   |  select_clause UNION all_or_distinct select_clause
--   |  select_clause INTERSECT all_or_distinct select_clause
--   |  select_clause EXCEPT all_or_distinct select_clause
-- @
data SimpleSelect row
  = NormalSimpleSelect
      (Targeting row)
      (Maybe IntoClause)
      (Maybe FromClause)
      (Maybe WhereClause)
      (Maybe GroupClause)
      (Maybe HavingClause)
      (Maybe WindowClause)
  | ValuesSimpleSelect (DeclareRow row) ValuesClause
  | TableSimpleSelect (DeclareRow row) RelationExpr
  | BinSimpleSelect SelectBinOp (SelectClause row) (Maybe Bool) (SelectClause row)

-- |
-- Covers these parts of spec:
--
-- ==== References
-- @
-- simple_select:
--   |  SELECT opt_all_clause opt_target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--   |  SELECT distinct_clause target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--
-- distinct_clause:
--   |  DISTINCT
--   |  DISTINCT ON '(' expr_list ')'
-- @
data Targeting row where
  EmptyTargeting :: Targeting '[]
  NormalTargeting :: TargetList row -> Targeting row
  AllTargeting :: TargetList row -> Targeting row
  DistinctTargeting :: Maybe ExprList -> TargetList row -> Targeting row

-- |
-- ==== References
-- @
-- target_list:
--   | target_el
--   | target_list ',' target_el
-- @
data TargetList row where
  TargetListOne :: TargetElRow row -> TargetList row
  TargetListCons :: TargetElRow row -> TargetList row' -> TargetList (row ++ row')

-- |
-- ==== References
-- @
-- target_el:
--   |  a_expr AS ColLabel
--   |  a_expr IDENT
--   |  a_expr
--   |  '*'
-- @
data TargetElRow row where
  TargetElRow :: DeclareRow (a ': row) -> TargetEl -> TargetElRow (a ': row)

-- |
-- ==== References
-- @
-- target_el:
--   |  a_expr AS ColLabel
--   |  a_expr IDENT
--   |  a_expr
--   |  '*'
-- @
data TargetEl
  = AliasedExprTargetEl AExpr Ident
  | ImplicitlyAliasedExprTargetEl AExpr Ident
  | ExprTargetEl AExpr
  | AsteriskTargetEl

-- |
-- ==== References
-- @
-- with_clause:
--   |  WITH cte_list
--   |  WITH_LA cte_list
--   |  WITH RECURSIVE cte_list
-- @
data WithClause = WithClause Bool (NonEmpty CommonTableExpr)

-- |
-- ==== References
-- @
-- common_table_expr:
--   |  name opt_name_list AS opt_materialized '(' PreparableStmt ')'
-- opt_materialized:
--   | MATERIALIZED
--   | NOT MATERIALIZED
--   | EMPTY
-- @
data CommonTableExpr = forall row. CommonTableExpr Ident (Maybe (NonEmpty Ident)) (Maybe Bool) (PreparableStmt row)

type IntoClause = OptTempTableName

-- |
-- ==== References
-- @
-- OptTempTableName:
--   |  TEMPORARY opt_table qualified_name
--   |  TEMP opt_table qualified_name
--   |  LOCAL TEMPORARY opt_table qualified_name
--   |  LOCAL TEMP opt_table qualified_name
--   |  GLOBAL TEMPORARY opt_table qualified_name
--   |  GLOBAL TEMP opt_table qualified_name
--   |  UNLOGGED opt_table qualified_name
--   |  TABLE qualified_name
--   |  qualified_name
-- @
data OptTempTableName
  = TemporaryOptTempTableName Bool QualifiedName
  | TempOptTempTableName Bool QualifiedName
  | LocalTemporaryOptTempTableName Bool QualifiedName
  | LocalTempOptTempTableName Bool QualifiedName
  | GlobalTemporaryOptTempTableName Bool QualifiedName
  | GlobalTempOptTempTableName Bool QualifiedName
  | UnloggedOptTempTableName Bool QualifiedName
  | TableOptTempTableName QualifiedName
  | QualifiedOptTempTableName QualifiedName

type FromClause = NonEmpty TableRef

type GroupClause = NonEmpty GroupByItem

-- |
-- ==== References
-- @
-- group_by_item:
--   |  a_expr
--   |  empty_grouping_set
--   |  cube_clause
--   |  rollup_clause
--   |  grouping_sets_clause
-- empty_grouping_set:
--   |  '(' ')'
-- rollup_clause:
--   |  ROLLUP '(' expr_list ')'
-- cube_clause:
--   |  CUBE '(' expr_list ')'
-- grouping_sets_clause:
--   |  GROUPING SETS '(' group_by_list ')'
-- @
data GroupByItem
  = ExprGroupByItem AExpr
  | EmptyGroupingSetGroupByItem
  | RollupGroupByItem ExprList
  | CubeGroupByItem ExprList
  | GroupingSetsGroupByItem (NonEmpty GroupByItem)

-- |
-- @
-- having_clause:
--   |  HAVING a_expr
--   |  EMPTY
-- @
type HavingClause = AExpr

-- |
-- @
-- window_clause:
--   |  WINDOW window_definition_list
--   |  EMPTY
--
-- window_definition_list:
--   |  window_definition
--   |  window_definition_list ',' window_definition
-- @
type WindowClause = NonEmpty WindowDefinition

-- |
-- @
-- window_definition:
--   |  ColId AS window_specification
-- @
data WindowDefinition = WindowDefinition Ident WindowSpecification

-- |
-- @
-- window_specification:
--   |  '(' opt_existing_window_name opt_partition_clause
--             opt_sort_clause opt_frame_clause ')'
--
-- opt_existing_window_name:
--   |  ColId
--   |  EMPTY
--
-- opt_partition_clause:
--   |  PARTITION BY expr_list
--   |  EMPTY
-- @
data WindowSpecification
  = WindowSpecification (Maybe ExistingWindowName) (Maybe PartitionClause) (Maybe SortClause) (Maybe FrameClause)

type PartitionClause = ExprList

-- |
-- ==== References
-- @
-- opt_frame_clause:
--   |  RANGE frame_extent opt_window_exclusion_clause
--   |  ROWS frame_extent opt_window_exclusion_clause
--   |  GROUPS frame_extent opt_window_exclusion_clause
--   |  EMPTY
-- @
data FrameClause = FrameClause FrameClauseMode FrameExtent (Maybe WindowExclusionClause)

-- |
-- ==== References
-- @
-- frame_extent:
--   |  frame_bound
--   |  BETWEEN frame_bound AND frame_bound
-- @
data FrameExtent = SingularFrameExtent FrameBound | BetweenFrameExtent FrameBound FrameBound

-- |
-- ==== References
-- @
-- frame_bound:
--   |  UNBOUNDED PRECEDING
--   |  UNBOUNDED FOLLOWING
--   |  CURRENT_P ROW
--   |  a_expr PRECEDING
--   |  a_expr FOLLOWING
-- @
data FrameBound
  = UnboundedPrecedingFrameBound
  | UnboundedFollowingFrameBound
  | CurrentRowFrameBound
  | PrecedingFrameBound AExpr
  | FollowingFrameBound AExpr

-- |
-- ==== References
-- @
-- values_clause:
--   |  VALUES '(' expr_list ')'
--   |  values_clause ',' '(' expr_list ')'
-- @
type ValuesClause = NonEmpty ExprList

-- |
--
-- sort_clause:
--   |  ORDER BY sortby_list
--
-- sortby_list:
--   |  sortby
--   |  sortby_list ',' sortby
type SortClause = NonEmpty SortBy

-- |
-- ==== References
-- @
-- sortby:
--   |  a_expr USING qual_all_Op opt_nulls_order
--   |  a_expr opt_asc_desc opt_nulls_order
-- @
data SortBy
  = UsingSortBy AExpr QualAllOp (Maybe NullsOrder)
  | AscDescSortBy AExpr (Maybe AscDesc) (Maybe NullsOrder)

-- |
-- ==== References
-- @
-- select_limit:
--   | limit_clause offset_clause
--   | offset_clause limit_clause
--   | limit_clause
--   | offset_clause
-- @
data SelectLimit
  = LimitOffsetSelectLimit LimitClause OffsetClause
  | OffsetLimitSelectLimit OffsetClause LimitClause
  | LimitSelectLimit LimitClause
  | OffsetSelectLimit OffsetClause

-- |
-- ==== References
-- @
-- limit_clause:
--   | LIMIT select_limit_value
--   | LIMIT select_limit_value ',' select_offset_value
--   | FETCH first_or_next select_fetch_first_value row_or_rows ONLY
--   | FETCH first_or_next row_or_rows ONLY
-- select_offset_value:
--   | a_expr
-- first_or_next:
--   | FIRST_P
--   | NEXT
-- row_or_rows:
--   | ROW
--   | ROWS
-- @
data LimitClause
  = LimitLimitClause SelectLimitValue (Maybe AExpr)
  | FetchOnlyLimitClause Bool (Maybe SelectFetchFirstValue) Bool

-- |
-- ==== References
-- @
-- select_fetch_first_value:
--   | c_expr
--   | '+' I_or_F_const
--   | '-' I_or_F_const
-- @
data SelectFetchFirstValue
  = ExprSelectFetchFirstValue CExpr
  | NumSelectFetchFirstValue Bool (Either Int64 Double)

-- |
-- ==== References
-- @
-- select_limit_value:
--   | a_expr
--   | ALL
-- @
data SelectLimitValue
  = ExprSelectLimitValue AExpr
  | AllSelectLimitValue

-- |
-- ==== References
-- @
-- offset_clause:
--   | OFFSET select_offset_value
--   | OFFSET select_fetch_first_value row_or_rows
-- select_offset_value:
--   | a_expr
-- row_or_rows:
--   | ROW
--   | ROWS
-- @
data OffsetClause
  = ExprOffsetClause AExpr
  | FetchFirstOffsetClause SelectFetchFirstValue Bool

-- * For Locking

-- |
-- ==== References
-- @
-- for_locking_clause:
--   | for_locking_items
--   | FOR READ ONLY
-- for_locking_items:
--   | for_locking_item
--   | for_locking_items for_locking_item
-- @
data ForLockingClause
  = ItemsForLockingClause (NonEmpty ForLockingItem)
  | ReadOnlyForLockingClause

-- |
-- ==== References
-- @
-- for_locking_item:
--   | for_locking_strength locked_rels_list opt_nowait_or_skip
-- locked_rels_list:
--   | OF qualified_name_list
--   | EMPTY
-- opt_nowait_or_skip:
--   | NOWAIT
--   | SKIP LOCKED
--   | EMPTY
-- @
data ForLockingItem = ForLockingItem ForLockingStrength (Maybe (NonEmpty QualifiedName)) (Maybe Bool)

-- * Table references and joining

-- |
-- ==== References
-- @
-- from_list:
--   | table_ref
--   | from_list ',' table_ref
-- @
type FromList = NonEmpty TableRef

-- |
-- ==== References
-- @
-- | relation_expr opt_alias_clause
-- | relation_expr opt_alias_clause tablesample_clause
-- | func_table func_alias_clause
-- | LATERAL_P func_table func_alias_clause
-- | xmltable opt_alias_clause
-- | LATERAL_P xmltable opt_alias_clause
-- | select_with_parens opt_alias_clause
-- | LATERAL_P select_with_parens opt_alias_clause
-- | joined_table
-- | '(' joined_table ')' alias_clause
--
-- TODO: Add xmltable
-- @
data TableRef
  = -- |
    -- @
    --    | relation_expr opt_alias_clause
    --    | relation_expr opt_alias_clause tablesample_clause
    -- @
    RelationExprTableRef RelationExpr (Maybe AliasClause) (Maybe TablesampleClause)
  | -- |
    -- @
    --    | func_table func_alias_clause
    --    | LATERAL_P func_table func_alias_clause
    -- @
    FuncTableRef Bool FuncTable (Maybe FuncAliasClause)
  | -- |
    -- @
    --    | select_with_parens opt_alias_clause
    --    | LATERAL_P select_with_parens opt_alias_clause
    -- @
    forall row. SelectTableRef Bool (SelectWithParens row) (Maybe AliasClause)
  | -- |
    -- @
    --    | joined_table
    --    | '(' joined_table ')' alias_clause
    -- @
    JoinTableRef JoinedTable (Maybe AliasClause)

-- |
-- ==== References
-- @
-- | qualified_name
-- | qualified_name '*'
-- | ONLY qualified_name
-- | ONLY '(' qualified_name ')'
-- @
data RelationExpr
  = SimpleRelationExpr
      QualifiedName
      -- ^ Name.
      Bool
      -- ^ Is asterisk present?
  | OnlyRelationExpr
      QualifiedName
      -- ^ Name.
      Bool
      -- ^ Are parentheses present?

-- |
-- ==== References
-- @
-- relation_expr_opt_alias:
--   | relation_expr
--   | relation_expr ColId
--   | relation_expr AS ColId
-- @
data RelationExprOptAlias = RelationExprOptAlias RelationExpr (Maybe (Bool, ColId))

-- |
-- ==== References
-- @
-- tablesample_clause:
--   | TABLESAMPLE func_name '(' expr_list ')' opt_repeatable_clause
-- @
data TablesampleClause = TablesampleClause FuncName ExprList (Maybe RepeatableClause)

-- |
-- ==== References
-- @
-- opt_repeatable_clause:
--   | REPEATABLE '(' a_expr ')'
--   | EMPTY
-- @
type RepeatableClause = AExpr

-- |
-- ==== References
-- @
-- func_table:
--   | func_expr_windowless opt_ordinality
--   | ROWS FROM '(' rowsfrom_list ')' opt_ordinality
-- @
data FuncTable
  = FuncExprFuncTable FuncExprWindowless OptOrdinality
  | RowsFromFuncTable RowsFromList OptOrdinality

-- |
-- ==== References
-- @
-- rowsfrom_item:
--   | func_expr_windowless opt_col_def_list
-- @
data RowsFromItem = RowsFromItem FuncExprWindowless (Maybe ColDefList)

-- |
-- ==== References
-- @
-- rowsfrom_list:
--   | rowsfrom_item
--   | rowsfrom_list ',' rowsfrom_item
-- @
type RowsFromList = NonEmpty RowsFromItem

-- |
-- ==== References
-- @
-- opt_col_def_list:
--   | AS '(' TableFuncElementList ')'
--   | EMPTY
-- @
type ColDefList = TableFuncElementList

-- |
-- ==== References
-- @
-- TableFuncElementList:
--   | TableFuncElement
--   | TableFuncElementList ',' TableFuncElement
-- @
type TableFuncElementList = NonEmpty TableFuncElement

-- |
-- ==== References
-- @
-- TableFuncElement:
--   | ColId Typename opt_collate_clause
-- @
data TableFuncElement = TableFuncElement ColId Typename (Maybe CollateClause)

-- |
-- ==== References
-- @
-- func_alias_clause:
--   | alias_clause
--   | AS '(' TableFuncElementList ')'
--   | AS ColId '(' TableFuncElementList ')'
--   | ColId '(' TableFuncElementList ')'
--   | EMPTY
-- @
data FuncAliasClause
  = AliasFuncAliasClause AliasClause
  | AsFuncAliasClause TableFuncElementList
  | AsColIdFuncAliasClause ColId TableFuncElementList
  | ColIdFuncAliasClause ColId TableFuncElementList

-- |
-- ==== References
-- @
-- | '(' joined_table ')'
-- | table_ref CROSS JOIN table_ref
-- | table_ref join_type JOIN table_ref join_qual
-- | table_ref JOIN table_ref join_qual
-- | table_ref NATURAL join_type JOIN table_ref
-- | table_ref NATURAL JOIN table_ref
--
-- The options are covered by the `JoinMeth` type.
-- @
data JoinedTable
  = InParensJoinedTable JoinedTable
  | MethJoinedTable JoinMeth TableRef TableRef

-- |
-- ==== References
-- @
-- | table_ref CROSS JOIN table_ref
-- | table_ref join_type JOIN table_ref join_qual
-- | table_ref JOIN table_ref join_qual
-- | table_ref NATURAL join_type JOIN table_ref
-- | table_ref NATURAL JOIN table_ref
-- @
data JoinMeth
  = CrossJoinMeth
  | QualJoinMeth (Maybe JoinType) JoinQual
  | NaturalJoinMeth (Maybe JoinType)

-- |
-- ==== References
-- @
-- join_qual:
--   |  USING '(' name_list ')'
--   |  ON a_expr
-- @
data JoinQual
  = UsingJoinQual (NonEmpty Ident)
  | OnJoinQual AExpr

-- * Where

type WhereClause = AExpr

-- |
-- ==== References
-- @
-- | WHERE a_expr
-- | WHERE CURRENT_P OF cursor_name
-- | /*EMPTY*/
-- @
data WhereOrCurrentClause
  = ExprWhereOrCurrentClause AExpr
  | CursorWhereOrCurrentClause CursorName

-- * Expression

type ExprList = NonEmpty AExpr

-- |
-- ==== References
-- @
-- a_expr:
--   | c_expr
--   | a_expr TYPECAST Typename
--   | a_expr COLLATE any_name
--   | a_expr AT TIME ZONE a_expr
--   | '+' a_expr
--   | '-' a_expr
--   | a_expr '+' a_expr
--   | a_expr '-' a_expr
--   | a_expr '*' a_expr
--   | a_expr '/' a_expr
--   | a_expr '%' a_expr
--   | a_expr '^' a_expr
--   | a_expr '<' a_expr
--   | a_expr '>' a_expr
--   | a_expr '=' a_expr
--   | a_expr LESS_EQUALS a_expr
--   | a_expr GREATER_EQUALS a_expr
--   | a_expr NOT_EQUALS a_expr
--   | a_expr qual_Op a_expr
--   | qual_Op a_expr
--   | a_expr qual_Op
--   | a_expr AND a_expr
--   | a_expr OR a_expr
--   | NOT a_expr
--   | NOT_LA a_expr
--   | a_expr LIKE a_expr
--   | a_expr LIKE a_expr ESCAPE a_expr
--   | a_expr NOT_LA LIKE a_expr
--   | a_expr NOT_LA LIKE a_expr ESCAPE a_expr
--   | a_expr ILIKE a_expr
--   | a_expr ILIKE a_expr ESCAPE a_expr
--   | a_expr NOT_LA ILIKE a_expr
--   | a_expr NOT_LA ILIKE a_expr ESCAPE a_expr
--   | a_expr SIMILAR TO a_expr
--   | a_expr SIMILAR TO a_expr ESCAPE a_expr
--   | a_expr NOT_LA SIMILAR TO a_expr
--   | a_expr NOT_LA SIMILAR TO a_expr ESCAPE a_expr
--   | a_expr IS NULL_P
--   | a_expr ISNULL
--   | a_expr IS NOT NULL_P
--   | a_expr NOTNULL
--   | row OVERLAPS row
--   | a_expr IS TRUE_P
--   | a_expr IS NOT TRUE_P
--   | a_expr IS FALSE_P
--   | a_expr IS NOT FALSE_P
--   | a_expr IS UNKNOWN
--   | a_expr IS NOT UNKNOWN
--   | a_expr IS DISTINCT FROM a_expr
--   | a_expr IS NOT DISTINCT FROM a_expr
--   | a_expr IS OF '(' type_list ')'
--   | a_expr IS NOT OF '(' type_list ')'
--   | a_expr BETWEEN opt_asymmetric b_expr AND a_expr
--   | a_expr NOT_LA BETWEEN opt_asymmetric b_expr AND a_expr
--   | a_expr BETWEEN SYMMETRIC b_expr AND a_expr
--   | a_expr NOT_LA BETWEEN SYMMETRIC b_expr AND a_expr
--   | a_expr IN_P in_expr
--   | a_expr NOT_LA IN_P in_expr
--   | a_expr subquery_Op sub_type select_with_parens
--   | a_expr subquery_Op sub_type '(' a_expr ')'
--   | UNIQUE select_with_parens
--   | a_expr IS DOCUMENT_P
--   | a_expr IS NOT DOCUMENT_P
--   | DEFAULT
-- @
data AExpr
  = CExprAExpr CExpr
  | TypecastAExpr AExpr Typename
  | CollateAExpr AExpr AnyName
  | AtTimeZoneAExpr AExpr AExpr
  | PlusAExpr AExpr
  | MinusAExpr AExpr
  | SymbolicBinOpAExpr AExpr SymbolicExprBinOp AExpr
  | PrefixQualOpAExpr QualOp AExpr
  | SuffixQualOpAExpr AExpr QualOp
  | AndAExpr AExpr AExpr
  | OrAExpr AExpr AExpr
  | NotAExpr AExpr
  | VerbalExprBinOpAExpr AExpr Bool VerbalExprBinOp AExpr (Maybe AExpr)
  | ReversableOpAExpr AExpr Bool AExprReversableOp
  | IsnullAExpr AExpr
  | NotnullAExpr AExpr
  | OverlapsAExpr Row Row
  | forall row. SubqueryAExpr AExpr SubqueryOp SubType (Either (SelectWithParens row) AExpr)
  | forall row. UniqueAExpr (SelectWithParens row)
  | DefaultAExpr

-- |
-- ==== References
-- @
-- b_expr:
--   | c_expr
--   | b_expr TYPECAST Typename
--   | '+' b_expr
--   | '-' b_expr
--   | b_expr '+' b_expr
--   | b_expr '-' b_expr
--   | b_expr '*' b_expr
--   | b_expr '/' b_expr
--   | b_expr '%' b_expr
--   | b_expr '^' b_expr
--   | b_expr '<' b_expr
--   | b_expr '>' b_expr
--   | b_expr '=' b_expr
--   | b_expr LESS_EQUALS b_expr
--   | b_expr GREATER_EQUALS b_expr
--   | b_expr NOT_EQUALS b_expr
--   | b_expr qual_Op b_expr
--   | qual_Op b_expr
--   | b_expr qual_Op
--   | b_expr IS DISTINCT FROM b_expr
--   | b_expr IS NOT DISTINCT FROM b_expr
--   | b_expr IS OF '(' type_list ')'
--   | b_expr IS NOT OF '(' type_list ')'
--   | b_expr IS DOCUMENT_P
--   | b_expr IS NOT DOCUMENT_P
-- @
data BExpr
  = CExprBExpr CExpr
  | TypecastBExpr BExpr Typename
  | PlusBExpr BExpr
  | MinusBExpr BExpr
  | SymbolicBinOpBExpr BExpr SymbolicExprBinOp BExpr
  | QualOpBExpr QualOp BExpr
  | IsOpBExpr BExpr Bool BExprIsOp

-- |
-- ==== References
-- @
-- c_expr:
--   | columnref
--   | AexprConst
--   | PARAM opt_indirection
--   | '(' a_expr ')' opt_indirection
--   | case_expr
--   | func_expr
--   | select_with_parens
--   | select_with_parens indirection
--   | EXISTS select_with_parens
--   | ARRAY select_with_parens
--   | ARRAY array_expr
--   | explicit_row
--   | implicit_row
--   | GROUPING '(' expr_list ')'
-- @
data CExpr
  = ColumnrefCExpr Columnref
  | AexprConstCExpr AexprConst
  | ParamCExpr (Encoders.Params ())
  | InParensCExpr AExpr (Maybe Indirection)
  | CaseCExpr CaseExpr
  | FuncCExpr FuncExpr
  | forall row. SelectWithParensCExpr (SelectWithParens row) (Maybe Indirection)
  | forall row. ExistsCExpr (SelectWithParens row)
  | forall row. ArrayCExpr (Either (SelectWithParens row) ArrayExpr)
  | ExplicitRowCExpr ExplicitRow
  | ImplicitRowCExpr ImplicitRow
  | GroupingCExpr ExprList

-- |
-- ==== References
-- @
-- in_expr:
--   | select_with_parens
--   | '(' expr_list ')'
-- @
data InExpr
  = forall row. SelectInExpr (SelectWithParens row)
  | ExprListInExpr ExprList

-- |
-- ==== References
-- @
-- array_expr:
--   | '[' expr_list ']'
--   | '[' array_expr_list ']'
--   | '[' ']'
-- @
data ArrayExpr
  = ExprListArrayExpr ExprList
  | ArrayExprListArrayExpr ArrayExprList
  | EmptyArrayExpr

-- |
-- ==== References
-- @
-- array_expr_list:
--   | array_expr
--   | array_expr_list ',' array_expr
-- @
type ArrayExprList = NonEmpty ArrayExpr

-- |
-- ==== References
-- @
-- row:
--   | ROW '(' expr_list ')'
--   | ROW '(' ')'
--   | '(' expr_list ',' a_expr ')'
-- @
data Row
  = ExplicitRowRow ExplicitRow
  | ImplicitRowRow ImplicitRow

-- |
-- ==== References
-- @
-- explicit_row:
--   | ROW '(' expr_list ')'
--   | ROW '(' ')'
-- @
type ExplicitRow = Maybe ExprList

-- |
-- ==== References
-- @
-- implicit_row:
--   | '(' expr_list ',' a_expr ')'
-- @
data ImplicitRow = ImplicitRow ExprList AExpr

-- |
-- ==== References
-- @
-- func_expr:
--   | func_application within_group_clause filter_clause over_clause
--   | func_expr_common_subexpr
-- @
data FuncExpr
  = ApplicationFuncExpr FuncApplication (Maybe WithinGroupClause) (Maybe FilterClause) (Maybe OverClause)
  | SubexprFuncExpr FuncExprCommonSubexpr

-- |
-- ==== References
-- @
-- func_expr_windowless:
--   | func_application
--   | func_expr_common_subexpr
-- @
data FuncExprWindowless
  = ApplicationFuncExprWindowless FuncApplication
  | CommonSubexprFuncExprWindowless FuncExprCommonSubexpr

-- |
-- ==== References
-- @
-- within_group_clause:
--   | WITHIN GROUP_P '(' sort_clause ')'
--   | EMPTY
-- @
type WithinGroupClause = SortClause

-- |
-- ==== References
-- @
-- filter_clause:
--   | FILTER '(' WHERE a_expr ')'
--   | EMPTY
-- @
type FilterClause = AExpr

-- |
-- ==== References
-- @
-- over_clause:
--   | OVER window_specification
--   | OVER ColId
--   | EMPTY
-- @
data OverClause
  = WindowOverClause WindowSpecification
  | ColIdOverClause ColId

-- |
-- ==== References
-- @
-- func_expr_common_subexpr:
--   | COLLATION FOR '(' a_expr ')'
--   | CURRENT_DATE
--   | CURRENT_TIME
--   | CURRENT_TIME '(' Iconst ')'
--   | CURRENT_TIMESTAMP
--   | CURRENT_TIMESTAMP '(' Iconst ')'
--   | LOCALTIME
--   | LOCALTIME '(' Iconst ')'
--   | LOCALTIMESTAMP
--   | LOCALTIMESTAMP '(' Iconst ')'
--   | CURRENT_ROLE
--   | CURRENT_USER
--   | SESSION_USER
--   | USER
--   | CURRENT_CATALOG
--   | CURRENT_SCHEMA
--   | CAST '(' a_expr AS Typename ')'
--   | EXTRACT '(' extract_list ')'
--   | OVERLAY '(' overlay_list ')'
--   | POSITION '(' position_list ')'
--   | SUBSTRING '(' substr_list ')'
--   | TREAT '(' a_expr AS Typename ')'
--   | TRIM '(' BOTH trim_list ')'
--   | TRIM '(' LEADING trim_list ')'
--   | TRIM '(' TRAILING trim_list ')'
--   | TRIM '(' trim_list ')'
--   | NULLIF '(' a_expr ',' a_expr ')'
--   | COALESCE '(' expr_list ')'
--   | GREATEST '(' expr_list ')'
--   | LEAST '(' expr_list ')'
--   | XMLCONCAT '(' expr_list ')'
--   | XMLELEMENT '(' NAME_P ColLabel ')'
--   | XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ')'
--   | XMLELEMENT '(' NAME_P ColLabel ',' expr_list ')'
--   | XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ',' expr_list ')'
--   | XMLEXISTS '(' c_expr xmlexists_argument ')'
--   | XMLFOREST '(' xml_attribute_list ')'
--   | XMLPARSE '(' document_or_content a_expr xml_whitespace_option ')'
--   | XMLPI '(' NAME_P ColLabel ')'
--   | XMLPI '(' NAME_P ColLabel ',' a_expr ')'
--   | XMLROOT '(' a_expr ',' xml_root_version opt_xml_root_standalone ')'
--   | XMLSERIALIZE '(' document_or_content a_expr AS SimpleTypename ')'
--
-- TODO: Implement the XML cases
-- @
data FuncExprCommonSubexpr
  = CollationForFuncExprCommonSubexpr AExpr
  | CurrentDateFuncExprCommonSubexpr
  | CurrentTimeFuncExprCommonSubexpr (Maybe Int64)
  | CurrentTimestampFuncExprCommonSubexpr (Maybe Int64)
  | LocalTimeFuncExprCommonSubexpr (Maybe Int64)
  | LocalTimestampFuncExprCommonSubexpr (Maybe Int64)
  | CurrentRoleFuncExprCommonSubexpr
  | CurrentUserFuncExprCommonSubexpr
  | SessionUserFuncExprCommonSubexpr
  | UserFuncExprCommonSubexpr
  | CurrentCatalogFuncExprCommonSubexpr
  | CurrentSchemaFuncExprCommonSubexpr
  | CastFuncExprCommonSubexpr AExpr Typename
  | ExtractFuncExprCommonSubexpr (Maybe ExtractList)
  | OverlayFuncExprCommonSubexpr OverlayList
  | PositionFuncExprCommonSubexpr (Maybe PositionList)
  | SubstringFuncExprCommonSubexpr (Maybe SubstrList)
  | TreatFuncExprCommonSubexpr AExpr Typename
  | TrimFuncExprCommonSubexpr (Maybe TrimModifier) TrimList
  | NullIfFuncExprCommonSubexpr AExpr AExpr
  | CoalesceFuncExprCommonSubexpr ExprList
  | GreatestFuncExprCommonSubexpr ExprList
  | LeastFuncExprCommonSubexpr ExprList

-- |
-- ==== References
-- @
-- extract_list:
--   | extract_arg FROM a_expr
--   | EMPTY
-- @
data ExtractList = ExtractList ExtractArg AExpr

-- |
-- ==== References
-- @
-- overlay_list:
--   | a_expr overlay_placing substr_from substr_for
--   | a_expr overlay_placing substr_from
-- @
data OverlayList = OverlayList AExpr OverlayPlacing SubstrFrom (Maybe SubstrFor)

-- |
-- ==== References
-- @
-- overlay_placing:
--   | PLACING a_expr
-- @
type OverlayPlacing = AExpr

-- |
-- ==== References
-- @
-- position_list:
--   | b_expr IN_P b_expr
--   | EMPTY
-- @
data PositionList = PositionList BExpr BExpr

-- |
-- ==== References
-- @
-- substr_list:
--   | a_expr substr_from substr_for
--   | a_expr substr_for substr_from
--   | a_expr substr_from
--   | a_expr substr_for
--   | expr_list
--   | EMPTY
-- @
data SubstrList
  = ExprSubstrList AExpr SubstrListFromFor
  | ExprListSubstrList ExprList

-- |
-- ==== References
-- @
--   | a_expr substr_from substr_for
--   | a_expr substr_for substr_from
--   | a_expr substr_from
--   | a_expr substr_for
-- @
data SubstrListFromFor
  = FromForSubstrListFromFor SubstrFrom SubstrFor
  | ForFromSubstrListFromFor SubstrFor SubstrFrom
  | FromSubstrListFromFor SubstrFrom
  | ForSubstrListFromFor SubstrFor

-- |
-- ==== References
-- @
-- substr_from:
--   | FROM a_expr
-- @
type SubstrFrom = AExpr

-- |
-- ==== References
-- @
-- substr_for:
--   | FOR a_expr
-- @
type SubstrFor = AExpr

-- |
-- ==== References
-- @
-- trim_list:
--   | a_expr FROM expr_list
--   | FROM expr_list
--   | expr_list
-- @
data TrimList
  = ExprFromExprListTrimList AExpr ExprList
  | FromExprListTrimList ExprList
  | ExprListTrimList ExprList

-- |
-- ==== References
-- @
-- case_expr:
--   | CASE case_arg when_clause_list case_default END_P
-- @
data CaseExpr = CaseExpr (Maybe CaseArg) WhenClauseList (Maybe CaseDefault)

-- |
-- ==== References
-- @
-- case_arg:
--   | a_expr
--   | EMPTY
-- @
type CaseArg = AExpr

-- |
-- ==== References
-- @
-- when_clause_list:
--   | when_clause
--   | when_clause_list when_clause
-- @
type WhenClauseList = NonEmpty WhenClause

-- |
-- ==== References
-- @
-- case_default:
--   | ELSE a_expr
--   | EMPTY
-- @
type CaseDefault = AExpr

-- |
-- ==== References
-- @
-- when_clause:
--   |  WHEN a_expr THEN a_expr
-- @
data WhenClause = WhenClause AExpr AExpr

-- |
-- ==== References
-- @
-- func_application:
--   |  func_name '(' ')'
--   |  func_name '(' func_arg_list opt_sort_clause ')'
--   |  func_name '(' VARIADIC func_arg_expr opt_sort_clause ')'
--   |  func_name '(' func_arg_list ',' VARIADIC func_arg_expr opt_sort_clause ')'
--   |  func_name '(' ALL func_arg_list opt_sort_clause ')'
--   |  func_name '(' DISTINCT func_arg_list opt_sort_clause ')'
--   |  func_name '(' '*' ')'
-- @
data FuncApplication = FuncApplication FuncName (Maybe FuncApplicationParams)

-- |
-- ==== References
-- @
-- func_application:
--   |  func_name '(' ')'
--   |  func_name '(' func_arg_list opt_sort_clause ')'
--   |  func_name '(' VARIADIC func_arg_expr opt_sort_clause ')'
--   |  func_name '(' func_arg_list ',' VARIADIC func_arg_expr opt_sort_clause ')'
--   |  func_name '(' ALL func_arg_list opt_sort_clause ')'
--   |  func_name '(' DISTINCT func_arg_list opt_sort_clause ')'
--   |  func_name '(' '*' ')'
-- @
data FuncApplicationParams
  = NormalFuncApplicationParams (Maybe Bool) (NonEmpty FuncArgExpr) (Maybe SortClause)
  | VariadicFuncApplicationParams (Maybe (NonEmpty FuncArgExpr)) FuncArgExpr (Maybe SortClause)
  | StarFuncApplicationParams

data FuncArgExpr
  = ExprFuncArgExpr AExpr
  | ColonEqualsFuncArgExpr Ident AExpr
  | EqualsGreaterFuncArgExpr Ident AExpr

-- * Constants

-- |
-- AexprConst:
--   |  Iconst
--   |  FCONST
--   |  Sconst
--   |  BCONST
--   |  XCONST
--   |  func_name Sconst
--   |  func_name '(' func_arg_list opt_sort_clause ')' Sconst
--   |  ConstTypename Sconst
--   |  ConstInterval Sconst opt_interval
--   |  ConstInterval '(' Iconst ')' Sconst
--   |  TRUE_P
--   |  FALSE_P
--   |  NULL_P
data AexprConst
  = IAexprConst Iconst
  | FAexprConst Fconst
  | SAexprConst Sconst
  | BAexprConst Bconst
  | XAexprConst Xconst
  | FuncAexprConst FuncName (Maybe FuncConstArgs) Sconst
  | ConstTypenameAexprConst ConstTypename Sconst
  | StringIntervalAexprConst Sconst (Maybe Interval)
  | IntIntervalAexprConst Iconst Sconst
  | BoolAexprConst Bool
  | NullAexprConst

-- |
-- ==== References
-- @
--   |  func_name '(' func_arg_list opt_sort_clause ')' Sconst
-- @
data FuncConstArgs = FuncConstArgs (NonEmpty FuncArgExpr) (Maybe SortClause)

-- |
-- ==== References
-- @
-- ConstTypename:
--   | Numeric
--   | ConstBit
--   | ConstCharacter
--   | ConstDatetime
-- @
data ConstTypename
  = NumericConstTypename Numeric
  | ConstBitConstTypename ConstBit
  | ConstCharacterConstTypename ConstCharacter
  | ConstDatetimeConstTypename ConstDatetime

-- |
-- ==== References
-- @
-- Numeric:
--   | INT_P
--   | INTEGER
--   | SMALLINT
--   | BIGINT
--   | REAL
--   | FLOAT_P opt_float
--   | DOUBLE_P PRECISION
--   | DECIMAL_P opt_type_modifiers
--   | DEC opt_type_modifiers
--   | NUMERIC opt_type_modifiers
--   | BOOLEAN_P
-- opt_float:
--   | '(' Iconst ')'
--   | EMPTY
-- opt_type_modifiers:
--   | '(' expr_list ')'
--   | EMPTY
-- @
data Numeric
  = IntNumeric
  | IntegerNumeric
  | SmallintNumeric
  | BigintNumeric
  | RealNumeric
  | FloatNumeric (Maybe Int64)
  | DoublePrecisionNumeric
  | DecimalNumeric (Maybe TypeModifiers)
  | DecNumeric (Maybe TypeModifiers)
  | NumericNumeric (Maybe TypeModifiers)
  | BooleanNumeric

-- |
-- ==== References
-- @
-- Bit:
--   | BitWithLength
--   | BitWithoutLength
-- ConstBit:
--   | BitWithLength
--   | BitWithoutLength
-- BitWithLength:
--   | BIT opt_varying '(' expr_list ')'
-- BitWithoutLength:
--   | BIT opt_varying
-- @
data Bit = Bit OptVarying (Maybe ExprList)

type ConstBit = Bit

-- * Names & References

-- |
-- ==== References
-- @
-- columnref:
--   | ColId
--   | ColId indirection
-- @
data Columnref = Columnref ColId (Maybe Indirection)

-- |
-- ==== References
-- @
-- func_name:
--   | type_function_name
--   | ColId indirection
-- @
data FuncName
  = TypeFuncName TypeFunctionName
  | IndirectedFuncName ColId Indirection

-- |
-- ==== References
-- @
-- columnref:
--   | ColId
--   | ColId indirection
-- qualified_name:
--   | ColId
--   | ColId indirection
-- @
data QualifiedName
  = SimpleQualifiedName Ident
  | IndirectedQualifiedName Ident Indirection

-- |
-- ==== References
-- @
-- indirection:
--   |  indirection_el
--   |  indirection indirection_el
-- @
type Indirection = NonEmpty IndirectionEl

-- |
-- ==== References
-- @
-- indirection_el:
--   |  '.' attr_name
--   |  '.' '*'
--   |  '[' a_expr ']'
--   |  '[' opt_slice_bound ':' opt_slice_bound ']'
-- opt_slice_bound:
--   |  a_expr
--   |  EMPTY
-- @
data IndirectionEl
  = AttrNameIndirectionEl Ident
  | AllIndirectionEl
  | ExprIndirectionEl AExpr
  | SliceIndirectionEl (Maybe AExpr) (Maybe AExpr)

-- * Types

-- |
-- Typename definition extended with custom question-marks for nullability specification.
--
-- To match the standard Postgres syntax simply interpret their presence as a parsing error.
--
-- ==== References
-- @
-- Typename:
--   | SimpleTypename opt_array_bounds
--   | SETOF SimpleTypename opt_array_bounds
--   | SimpleTypename ARRAY '[' Iconst ']'
--   | SETOF SimpleTypename ARRAY '[' Iconst ']'
--   | SimpleTypename ARRAY
--   | SETOF SimpleTypename ARRAY
-- @
data Typename
  = Typename
      Bool
      -- ^ SETOF
      SimpleTypename
      Bool
      -- ^ Question mark
      (Maybe (TypenameArrayDimensions, Bool))
      -- ^ Array dimensions possibly followed by a question mark

-- |
-- ==== References
-- @
-- SimpleTypename:
--   | GenericType
--   | Numeric
--   | Bit
--   | Character
--   | ConstDatetime
--   | ConstInterval opt_interval
--   | ConstInterval '(' Iconst ')'
-- ConstInterval:
--   | INTERVAL
-- @
data SimpleTypename
  = GenericTypeSimpleTypename GenericType
  | NumericSimpleTypename Numeric
  | BitSimpleTypename Bit
  | CharacterSimpleTypename Character
  | ConstDatetimeSimpleTypename ConstDatetime
  | ConstIntervalSimpleTypename (Either (Maybe Interval) Iconst)

-- |
-- ==== References
-- @
-- GenericType:
--   | type_function_name opt_type_modifiers
--   | type_function_name attrs opt_type_modifiers
-- @
data GenericType = GenericType TypeFunctionName (Maybe Attrs) (Maybe TypeModifiers)

-- |
-- ==== References
-- @
-- opt_type_modifiers:
--   | '(' expr_list ')'
--   | EMPTY
-- @
type TypeModifiers = ExprList

-- |
-- ==== References
-- @
-- type_list:
--   | Typename
--   | type_list ',' Typename
-- @
type TypeList = NonEmpty Typename

-- * Operators

-- |
-- ==== References
-- @
--   | a_expr IS NULL_P
--   | a_expr IS TRUE_P
--   | a_expr IS FALSE_P
--   | a_expr IS UNKNOWN
--   | a_expr IS DISTINCT FROM a_expr
--   | a_expr IS OF '(' type_list ')'
--   | a_expr BETWEEN opt_asymmetric b_expr AND a_expr
--   | a_expr BETWEEN SYMMETRIC b_expr AND a_expr
--   | a_expr IN_P in_expr
--   | a_expr IS DOCUMENT_P
-- @
data AExprReversableOp
  = NullAExprReversableOp
  | TrueAExprReversableOp
  | FalseAExprReversableOp
  | UnknownAExprReversableOp
  | DistinctFromAExprReversableOp AExpr
  | OfAExprReversableOp TypeList
  | BetweenAExprReversableOp Bool BExpr AExpr
  | BetweenSymmetricAExprReversableOp BExpr AExpr
  | InAExprReversableOp InExpr
  | DocumentAExprReversableOp

-- |
-- ==== References
-- @
--   | b_expr IS DISTINCT FROM b_expr
--   | b_expr IS NOT DISTINCT FROM b_expr
--   | b_expr IS OF '(' type_list ')'
--   | b_expr IS NOT OF '(' type_list ')'
--   | b_expr IS DOCUMENT_P
--   | b_expr IS NOT DOCUMENT_P
-- @
data BExprIsOp
  = DistinctFromBExprIsOp BExpr
  | OfBExprIsOp TypeList
  | DocumentBExprIsOp

-- * Indexes

-- |
-- ==== References
-- @
-- index_params:
--   | index_elem
--   | index_params ',' index_elem
-- @
type IndexParams = NonEmpty IndexElem

-- |
-- ==== References
-- @
-- index_elem:
--   | ColId opt_collate opt_class opt_asc_desc opt_nulls_order
--   | func_expr_windowless opt_collate opt_class opt_asc_desc opt_nulls_order
--   | '(' a_expr ')' opt_collate opt_class opt_asc_desc opt_nulls_order
-- @
data IndexElem = IndexElem IndexElemDef (Maybe Collate) (Maybe Class) (Maybe AscDesc) (Maybe NullsOrder)

-- |
-- ==== References
-- @
--   | ColId opt_collate opt_class opt_asc_desc opt_nulls_order
--   | func_expr_windowless opt_collate opt_class opt_asc_desc opt_nulls_order
--   | '(' a_expr ')' opt_collate opt_class opt_asc_desc opt_nulls_order
-- @
data IndexElemDef
  = IdIndexElemDef ColId
  | FuncIndexElemDef FuncExprWindowless
  | ExprIndexElemDef AExpr
