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
--   name :: String,
--   age :: Int,
--   email :: String
-- } deriving (Eq, Show, Generic)
--
-- instance Queryable Person
--
-- -- Query the data
-- youngPeople = whereQ people "age < 30"
-- bobOrAlice = whereQ people "name = 'Bob' or name = 'Alice'"
-- @
--
-- = Nested queries with ANY/ALL
--
-- For querying into nested collections, use the ANY or ALL:
--
-- @
-- data Transaction = Transaction {
--   amount :: Double,
--   items :: [Item]
-- } deriving (Eq, Show, Generic, Typeable)
--
-- instance Queryable Transaction
-- instance Queryable Item
--
-- -- Query with ANY/ALL
-- hasExpensiveItem = whereQ transactions "ANY(items.price) > 100"
-- allInStock = whereQ transactions "ALL(items.inStock)"
-- @

module TypeQL 
  (
    whereQ
  , module TypeQL.Queryable
  , Generic
  ) where

import TypeQL.Core
import TypeQL.Queryable
import GHC.Generics (Generic)
