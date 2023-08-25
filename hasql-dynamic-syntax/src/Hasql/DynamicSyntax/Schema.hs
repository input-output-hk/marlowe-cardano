{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasql.DynamicSyntax.Schema (
  AllNull,
  Column (..),
  ColumnName,
  Columns (..),
  GetColumnType,
  SingColumns (..),
  Table (..),
  TableRow,
  allNull,
  columnNameVal,
  cte',
  cte,
  materializedCte,
  singColumn,
  singColumnName,
  singTable,
  tableRef,
  tableRefAlias,
  tableRelationExpr,
  tableColumn,
  wildcard,
) where

import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.Type.Equality (TestEquality (testEquality), type (:~:) (..))
import GHC.Exts (IsString (..))
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, sameSymbol, symbolVal)
import Hasql.DynamicSyntax.Ast
import PostgresqlSyntax.Ast (AliasClause (..), Ident)
import Unsafe.Coerce (unsafeCoerce)

data Table cols = Table
  { tableName :: !Ident
  , tableSchema :: !(Maybe Ident)
  , tableColumns :: Columns cols
  }
  deriving (Show, Eq, Ord)

singTable :: (SingColumns cols) => Ident -> Maybe Ident -> Table cols
singTable tableName tableSchema = Table tableName tableSchema $ singColumns tableName

instance IsTableRef (Table cols) where
  toTableRef = tableRef

instance IsTargetElRow (Table (col ': cols)) where
  type TargetRow (Table (col ': cols)) = TableRow (col ': cols)
  toTargetElRow Table{..} =
    TargetElRow (tableRows tableColumns) $
      Columnref tableName $
        Just $
          pure AllIndirectionEl

wildcard :: Table (col ': cols) -> Targeting '[TableRow (col ': cols)]
wildcard Table{..} =
  NormalTargeting $
    TargetElRow (tableRows tableColumns) AsteriskTargetEl
      :. TargetListNil

data Columns cols where
  ColumnsNil :: Columns '[]
  ColumnsCons :: !(Column col) -> Columns cols -> Columns (col ': cols)

class SingColumns cols where singColumns :: Ident -> Columns cols
instance SingColumns '[] where singColumns _ = ColumnsNil
instance (SingColumn col, SingColumns cols) => SingColumns (col ': cols) where
  singColumns tableName = ColumnsCons (singColumn tableName) $ singColumns tableName

deriving instance Show (Columns cols)
deriving instance Eq (Columns cols)
deriving instance Ord (Columns cols)

type ColumnK = (Symbol, Type, Type)

data Column col where
  Column
    :: !Ident
    -> !(ColumnName name)
    -> !(SqlType type_)
    -> !(Nullability nullability)
    -> Column '(name, type_, nullability)

deriving instance Show (Column col)
deriving instance Eq (Column col)
deriving instance Ord (Column col)

instance IsTargetElRow (Column '(name, t, n)) where
  type TargetRow (Column '(name, t, n)) = '[ '(t, n)]
  toTargetElRow col@(Column _ _ t n) =
    TargetElRow (TargetTypesCons (ColumnType t n) TargetTypesNil) col

instance IsCExpr (Column col) where
  toCExpr (Column tableName name _ _) =
    toCExpr $
      Columnref tableName $
        Just $
          pure $
            AttrNameIndirectionEl $
              columnNameVal name

instance IsFuncArgExpr (Column col) where toFuncArgExpr = toFuncArgExpr . toCExpr
instance IsIndirectionEl (Column col) where toIndirectionEl = toIndirectionEl . toCExpr
instance IsIndexElemDef (Column col) where toIndexElemDef = toIndexElemDef . toCExpr
instance IsGroupByItem (Column col) where toGroupByItem = toGroupByItem . toCExpr
instance IsSelectFetchFirstValue (Column col) where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectLimitValue (Column col) where toSelectLimitValue = toSelectLimitValue . toCExpr
instance IsOffsetClause (Column col) where toOffsetClause = toOffsetClause . toCExpr
instance IsWhereOrCurrentClause (Column col) where toWhereOrCurrentClause = toWhereOrCurrentClause . toCExpr
instance IsAExpr (Column col) where toAExpr = toAExpr . toCExpr
instance IsBExpr (Column col) where toBExpr = toBExpr . toCExpr
instance IsTargetEl (Column col)
instance IsLimitClause (Column col)

class SingColumn col where singColumn :: Ident -> Column col
instance (KnownSymbol name, SingColumnType t nullability) => SingColumn '(name, t, nullability) where
  singColumn tableName = Column tableName singColumnName singSqlType singNullability

data ColumnName name where
  UnsafeColumnName :: (KnownSymbol name) => !Ident -> ColumnName name

deriving instance Show (ColumnName name)
deriving instance Eq (ColumnName name)
deriving instance Ord (ColumnName name)

instance TestEquality ColumnName where
  testEquality :: forall a b. ColumnName a -> ColumnName b -> Maybe (a :~: b)
  testEquality UnsafeColumnName{} UnsafeColumnName{} = sameSymbol (Proxy @a) (Proxy @b)

singColumnName :: forall name. (KnownSymbol name) => ColumnName name
singColumnName = UnsafeColumnName $ fromString $ symbolVal $ Proxy @name

columnNameVal :: ColumnName name -> Ident
columnNameVal (UnsafeColumnName ident) = ident

cte' :: Maybe Bool -> Table cols -> PreparableStmt '[TableRow cols] -> CommonTableExpr
cte' materialized Table{..} =
  CommonTableExpr
    tableName
    ( case tableColumnNames tableColumns of
        [] -> Nothing
        col : cols -> Just $ col NE.:| cols
    )
    materialized

tableColumnNames :: Columns cols -> [Ident]
tableColumnNames ColumnsNil = []
tableColumnNames (ColumnsCons (Column _ name _ _) cols) = columnNameVal name : tableColumnNames cols

cte :: Table cols -> PreparableStmt '[TableRow cols] -> CommonTableExpr
cte = cte' Nothing

materializedCte :: Table cols -> PreparableStmt '[TableRow cols] -> CommonTableExpr
materializedCte = cte' $ Just True

-- | Produces the table ref "[schemaName.]tableName".
tableRef :: Table cols -> TableRef
tableRef table = RelationExprTableRef (tableRelationExpr table) Nothing Nothing

-- | Produces the relation expression "[schemaName.]tableName".
tableRelationExpr :: Table cols -> RelationExpr
tableRelationExpr Table{..} =
  SimpleRelationExpr
    ( case tableSchema of
        Nothing -> SimpleQualifiedName tableName
        Just schema -> IndirectedQualifiedName schema $ pure $ AttrNameIndirectionEl tableName
    )
    False

-- | Produces the table ref "schemaName.tableName AS alias" and a new table for resolving columns.
tableRefAlias :: Table cols -> Ident -> (Table cols, TableRef)
tableRefAlias table alias =
  ( table{tableName = alias, tableSchema = Nothing}
  , RelationExprTableRef (tableRelationExpr table) (Just $ AliasClause True alias Nothing) Nothing
  )

type family GetColumn (name :: Symbol) (cols :: [ColumnK]) :: ColumnK where
  GetColumn name '[] = TypeError ('Text "No such column: " ':<>: 'ShowType name)
  GetColumn name ('(name, t, n) ': _) = '(name, t, n)
  GetColumn name (_ ': cols) = GetColumn name cols

class HasColumn name cols where
  getColumn :: ColumnName name -> Columns cols -> Column (GetColumn name cols)

instance (TypeError ('Text "No such column: " ':<>: 'ShowType name)) => HasColumn name '[] where
  getColumn _ _ = undefined

instance {-# OVERLAPPING #-} HasColumn name ('(name, t, n) ': cols) where
  getColumn _ (ColumnsCons col _) = col

instance {-# OVERLAPPING #-} (HasColumn name cols) => HasColumn name (col ': cols) where
  getColumn name (ColumnsCons _ cols) = unsafeCoerce $ getColumn name cols

tableColumn :: forall name cols. (KnownSymbol name, HasColumn name cols) => Table cols -> Column (GetColumn name cols)
tableColumn Table{..} = getColumn (singColumnName @name) tableColumns

tableRows :: Columns cols -> TargetTypes (TableRow cols)
tableRows ColumnsNil = TargetTypesNil
tableRows (ColumnsCons (Column _ _ t nullability) cols) = TargetTypesCons (ColumnType t nullability) $ tableRows cols

type family TableRow (cols :: [ColumnK]) :: [(Type, Type)] where
  TableRow '[] = '[]
  TableRow (col ': cols) = GetColumnType col ': TableRow cols

type family GetColumnType (col :: ColumnK) :: (Type, Type) where
  GetColumnType '(_, t, nullability) = '(t, nullability)

type family AllNull (cols :: [ColumnK]) :: [ColumnK] where
  AllNull '[] = '[]
  AllNull ('(name, t, _) ': cols) = ('(name, t, Null) ': AllNull cols)

allNull :: Table cols -> Table (AllNull cols)
allNull Table{..} = Table{tableColumns = allNullColumns tableColumns, ..}

allNullColumns :: Columns cols -> Columns (AllNull cols)
allNullColumns ColumnsNil = ColumnsNil
allNullColumns (ColumnsCons (Column tableName name t _) cols) = ColumnsCons (Column tableName name t Null) $ allNullColumns cols
