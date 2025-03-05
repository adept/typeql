# TypeQL

TypeQL is a library for SQL-like querying of Haskell data structures.

## Overview

TypeQL leverages Haskell's type system and Generic typeclass to let you query your data structures with familiar SQL-like syntax:

```haskell
-- Find all transactions from January with "invoice" tag
let results = whereQ transactions "tdate between '2025-01-01' and '2025-01-31' and ttags = 'invoice'" transactions

-- Find high-value transactions
let highValue = whereQ transactions "tindex > 1000 or ANY(tpostings.pamount) > 1000"
```

## Features

- **Data Type-driven**: Field names that you can use in queries are derived from your data type definitions
- **Generic implementation**: Automatic derivation for most data types (via `deriving Generic`)
- **Minimal boilerplate**: Leverages `Read`/`Show` for most types
- **Right syntax**: Supports =, !=, <, >, <=, >=, NOT, BETWEEN, IN, ALL, ANY
- **Support nested records**: Reach into nested records with dot-separated field path syntax: `field.subfield.subsubfield`

## Installation

```
stack build
```

## Running Tests

```
stack test
```

## Usage Example

```haskell
{-# LANGUAGE DeriveGeneric #-}

import TypeQL
import GHC.Generics
import Data.Text (Text)
import Data.Typeable

-- Define a simple data type with a nested list
data LineItem = LineItem {
  itemName :: Text,
  itemPrice :: Double
} deriving (Eq, Show, Read, Generic)

data Order = Order {
  orderId :: Integer,
  customerName :: Text,
  items :: [LineItem]  -- Nested list we want to query
} deriving (Eq, Show, Generic)

instance Literal Text where
  toString = unpack
  fromString = pack

instance Queryable LineItem
instance Queryable Order

expensiveOrders = whereQ orders "ANY(items.itemPrice) > 100"
```

For more examples, see `QuerySpec.hs`in the `test` folder.

## License

BSD-3-Clause
