{-# LANGUAGE FlexibleContexts #-}

-- | TypeQL: Type-safe SQL-like queries for Haskell data structures
--
-- This library allows you to query Haskell data structures using SQL-like syntax.
-- It leverages GHC.Generics to automatically derive query capabilities for your
-- data types with minimal boilerplate.
--
-- = Basic usage
--
-- @
-- data Person = Person {
--   name :: Text,
--   age :: Int,
--   email :: Text
-- } deriving (Eq, Show, Generic, Typeable)
--
-- instance 'Queryable' Person
--
-- -- Query the data
-- youngPeople = 'where_' "age < 30" people
-- bobOrAlice = 'where_' "name = 'Bob' or name = 'Alice'" people
-- @
--
-- = Nested queries with ANY/ALL
--
-- For querying into nested collections, use the quantifiers:
--
-- @
-- data Transaction = Transaction {
--   amount :: Double,
--   items :: [Item]
-- } deriving (Eq, Show, Generic, Typeable)
--
-- instance 'NestedFieldAccess' Transaction  -- Implement this for nested querying
-- instance 'QuantifiableRecord' Transaction  -- Enable quantifiers
--
-- -- Query with ANY/ALL
-- hasExpensiveItem = 'whereQ' "ANY(items.price) > 100" transactions
-- allInStock = 'whereQ' "ALL(items.inStock) = true" transactions
-- @

module TypeQL 
  ( -- * Basic querying
    whereQ
  , Queryable
  
    -- * Re-exports
  , Generic
  ) where

import TypeQL.Core
import TypeQL.AST()
import TypeQL.Queryable (Queryable)
import GHC.Generics (Generic)
