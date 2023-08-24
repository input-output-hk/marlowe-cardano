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
  ColumnName,
  Columns (..),
  GetColumn,
  GetColumnType,
  HasColumn (..),
  Table (..),
  TableRow,
  column,
  columnNameVal,
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
  (:.) :: !(Column name type_) -> Columns cols -> Columns ('(name, type_) ': cols)

deriving instance Show (Columns cols)
deriving instance Eq (Columns cols)
deriving instance Ord (Columns cols)

data Column name type_ = Column
  { columnName :: !(ColumnName name)
  , columnType :: !(ColumnType type_)
  }
  deriving (Show, Eq, Ord)

column :: (KnownSymbol name) => ColumnType type_ -> Column name type_
column = Column mkColumnName

data ColumnName name where
  UnsafeColumnName :: (KnownSymbol name) => !Ident -> ColumnName name

deriving instance Show (ColumnName name)
deriving instance Eq (ColumnName name)
deriving instance Ord (ColumnName name)

mkColumnName :: forall name. (KnownSymbol name) => ColumnName name
mkColumnName = UnsafeColumnName $ fromString $ symbolVal $ Proxy @name

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

class HasColumn (name :: Symbol) (cols :: [(Symbol, (Type, Type))]) where
  getColumn :: ColumnName name -> Columns cols -> ColumnType (GetColumn name cols)

type family GetColumn (name :: Symbol) (cols :: [(Symbol, (Type, Type))]) :: (Type, Type) where
  GetColumn name '[] = TypeError ('Text "No such column: " ':<>: 'ShowType name)
  GetColumn name ('(name, type_) ': cols) = type_
  GetColumn name ('(name', type_) ': cols) = GetColumn name cols

instance (TypeError ('Text "Table is empty")) => HasColumn name '[] where
  getColumn _ _ = error "Table is empty"

instance {-# OVERLAPPING #-} HasColumn name ('(name, type_) ': cols) where
  getColumn _ (Column{..} :. _) = columnType

instance {-# OVERLAPPING #-} (HasColumn name cols) => HasColumn name ('(name', type_) ': cols) where
  getColumn name (_ :. cols) = unsafeCoerce $ getColumn name cols

tableRows :: Columns cols -> DeclareRow (TableRow cols)
tableRows ColumnsNil = DeclareNil
tableRows (Column{..} :. cols) = DeclareCons columnType $ tableRows cols

type family TableRow (cols :: [(Symbol, (Type, Type))]) :: [(Type, Type)] where
  TableRow '[] = '[]
  TableRow ('(_, col) ': cols) = col ': TableRow cols

type family GetColumnType (col :: (Symbol, (Type, Type))) :: (Type, Type) where
  GetColumnType '(_, col) = col
