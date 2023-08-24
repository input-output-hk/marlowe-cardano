{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hasql.DynamicSyntax.Schema (
  Column (..),
  SColumn (..),
  ColumnName,
  Columns (..),
  GetColumn,
  GetColumnType,
  HasColumn (..),
  Table (..),
  TableRow,
  SingColumns (..),
  columnNameVal,
  singColumn,
  singColumnName,
  tableAll,
  tableColumn,
  tableColumnAlias,
  tableRef,
  tableRefAlias,
  tableRelationExpr,
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import qualified Data.Text as T
import GHC.Exts (IsString (..))
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import Hasql.DynamicSyntax.Ast
import PostgresqlSyntax.Ast (AliasClause (..), Ident)
import qualified PostgresqlSyntax.Parsing as Parsing
import Unsafe.Coerce (unsafeCoerce)

data Table cols = Table
  { tableName :: !Ident
  , tableSchema :: !(Maybe Ident)
  , tableColumns :: Columns cols
  }
  deriving (Show, Eq, Ord)

data Columns cols where
  ColumnsNil :: Columns '[]
  (:.) :: !(SColumn col) -> Columns cols -> Columns (col ': cols)

class SingColumns cols where singColumns :: Columns cols
instance SingColumns '[] where singColumns = ColumnsNil
instance (SingColumn col, SingColumns cols) => SingColumns (col ': cols) where
  singColumns = singColumn :. singColumns

deriving instance Show (Columns cols)
deriving instance Eq (Columns cols)
deriving instance Ord (Columns cols)

data Column = Column Symbol Type Type

data SColumn col where
  SColumn
    :: !(ColumnName name)
    -> !(SqlType type_)
    -> !(Nullability nullability)
    -> SColumn ('Column name type_ nullability)

deriving instance Show (SColumn col)
deriving instance Eq (SColumn col)
deriving instance Ord (SColumn col)

class SingColumn col where singColumn :: SColumn col
instance (KnownSymbol name, SingColumnType t nullability) => SingColumn ('Column name t nullability) where
  singColumn = SColumn singColumnName singSqlType singNullability

data ColumnName name where
  UnsafeColumnName :: (KnownSymbol name) => !Ident -> ColumnName name

deriving instance Show (ColumnName name)
deriving instance Eq (ColumnName name)
deriving instance Ord (ColumnName name)

singColumnName :: forall name. (KnownSymbol name) => ColumnName name
singColumnName = UnsafeColumnName $ fromString $ symbolVal $ Proxy @name

columnNameVal :: ColumnName name -> Ident
columnNameVal (UnsafeColumnName ident) = ident

instance IsString Ident where
  fromString = either error id . Parsing.run Parsing.ident . T.pack

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

-- | Produces the target "tableName.*" with the row type derived from the table.
tableAll :: Table (col : cols) -> TargetElRow (GetColumnType col ': TableRow cols)
tableAll Table{..} = case tableColumns of
  _ :. _ ->
    TargetElRow (tableRows tableColumns) $
      ExprTargetEl $
        CExprAExpr $
          ColumnrefCExpr $
            Columnref tableName $
              Just $
                pure AllIndirectionEl

-- | Produces the target "tableName.columnName" with the row type derived from the column.
tableColumn
  :: (HasColumn name cols)
  => ColumnName name
  -> Table cols
  -> TargetElRow '[GetColumn name cols]
tableColumn name Table{..} =
  TargetElRow (DeclareCons (getColumn name tableColumns) DeclareNil) $
    ExprTargetEl $
      CExprAExpr $
        ColumnrefCExpr $
          Columnref tableName $
            Just $
              pure $
                AttrNameIndirectionEl $
                  columnNameVal name

-- | Produces the target "tableName.columnName AS alias" with the row type derived from the column.
tableColumnAlias
  :: (HasColumn name cols)
  => ColumnName name
  -> Ident
  -> Table cols
  -> TargetElRow '[GetColumn name cols]
tableColumnAlias name alias Table{..} =
  TargetElRow (DeclareCons (getColumn name tableColumns) DeclareNil) $
    AliasedExprTargetEl
      ( CExprAExpr $
          ColumnrefCExpr $
            Columnref tableName $
              Just $
                pure $
                  AttrNameIndirectionEl $
                    columnNameVal name
      )
      alias

class HasColumn (name :: Symbol) (cols :: [Column]) where
  getColumn :: ColumnName name -> Columns cols -> ColumnType (GetColumn name cols)

type family GetColumn (name :: Symbol) (cols :: [Column]) :: (Type, Type) where
  GetColumn name '[] = TypeError ('Text "No such column: " ':<>: 'ShowType name)
  GetColumn name ('Column name type_ nullability ': cols) = '(type_, nullability)
  GetColumn name (col ': cols) = GetColumn name cols

instance (TypeError ('Text "No such column: " ':<>: 'ShowType name)) => HasColumn name '[] where
  getColumn _ _ = undefined

instance {-# OVERLAPPING #-} HasColumn name ('Column name type_ nullability ': cols) where
  getColumn _ (SColumn _ t n :. _) = ColumnType t n

instance {-# OVERLAPPING #-} (HasColumn name cols) => HasColumn name (col ': cols) where
  getColumn name (_ :. cols) = unsafeCoerce $ getColumn name cols

tableRows :: Columns cols -> DeclareRow (TableRow cols)
tableRows ColumnsNil = DeclareNil
tableRows (SColumn _ t nullability :. cols) = DeclareCons (ColumnType t nullability) $ tableRows cols

type family TableRow (cols :: [Column]) :: [(Type, Type)] where
  TableRow '[] = '[]
  TableRow (col ': cols) = GetColumnType col ': TableRow cols

type family GetColumnType (col :: Column) :: (Type, Type) where
  GetColumnType ('Column _ t nullability) = '(t, nullability)
