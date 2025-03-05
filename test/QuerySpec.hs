{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module QuerySpec (spec) where

import Test.Hspec
import TypeQL (whereQ, Queryable, Generic)
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.Calendar (fromGregorian)

-- Data Types for Testing

-- Simple enum type with automatic derivation
data PaymentStatus = Pending | Completed | Refunded | Cancelled
  deriving (Eq, Show, Read, Generic)

-- No implementation needed due to Read/Show defaults
instance Queryable PaymentStatus

-- Line item in an order
data LineItem = LineItem {
  itemCode :: Text,
  itemName :: Text,
  itemPrice :: Double,
  itemQuantity :: Int,
  itemInStock :: Bool
} deriving (Eq, Show, Read, Generic)

instance Queryable LineItem

-- Order with line items
data Order = Order {
  orderId :: Integer,
  orderDate :: Day,
  customerId :: Text,
  status :: PaymentStatus,
  orderTotal :: Double,
  items :: [LineItem]
} deriving (Eq, Show, Generic)

instance Queryable Order

-- Sample data
sampleOrders :: [Order]
sampleOrders = [
  Order 1001 (fromGregorian 2025 1 10) "CUST001" Completed 145.99 
    [ LineItem "PROD001" "Wireless Earbuds" 89.99 1 True
    , LineItem "PROD002" "USB Cable" 12.99 2 True
    , LineItem "PROD003" "Phone Case" 29.99 1 True
    ],
  
  Order 1002 (fromGregorian 2025 1 15) "CUST002" Completed 399.99
    [ LineItem "PROD004" "Tablet" 399.99 1 True
    ],
    
  Order 1003 (fromGregorian 2025 1 20) "CUST001" Pending 219.97
    [ LineItem "PROD005" "Bluetooth Speaker" 129.99 1 True
    , LineItem "PROD006" "Power Bank" 59.99 1 False
    , LineItem "PROD007" "Screen Protector" 29.99 1 True
    ],
    
  Order 1004 (fromGregorian 2025 2 5) "CUST003" Cancelled 799.99
    [ LineItem "PROD008" "Laptop" 799.99 1 True
    ],
    
  Order 1005 (fromGregorian 2025 2 12) "CUST002" Completed 724.95
    [ LineItem "PROD009" "Smartwatch" 249.99 1 True
    , LineItem "PROD010" "Wireless Headphones" 199.99 1 True
    , LineItem "PROD011" "External Hard Drive" 119.99 1 True
    , LineItem "PROD012" "Wireless Mouse" 49.99 1 True
    , LineItem "PROD013" "Wireless Keyboard" 89.99 1 True
    ]
  ]


spec :: Spec
spec = describe "TypeQL Queries" $ do
    let orders = sampleOrders
    
    it "simple equality" $ do
      let completedOrders = whereQ orders "status = 'Completed'"
      length completedOrders `shouldBe` 3
    
    it "inequality, single field" $ do
      let januaryOrders = whereQ orders "orderDate >= '2025-01-01' and orderDate < '2025-02-01'"
      length januaryOrders `shouldBe` 3
    
    it "AND of two conditions" $ do
      let expensiveCompletedOrders = whereQ orders "orderTotal > 500 and status = 'Completed'"
      length expensiveCompletedOrders `shouldBe` 1
    
    it "ANY + equality" $ do
      let outOfStockOrders = whereQ orders "ANY(items.itemInStock) = false"
      length outOfStockOrders `shouldBe` 1

    it "bare ALL" $ do
      let outOfStockOrders = whereQ orders "ALL(items.itemInStock)"
      length outOfStockOrders `shouldBe` 4

    it "bare ANY" $ do
      let outOfStockOrders = whereQ orders "ANY(items.itemInStock)"
      length outOfStockOrders `shouldBe` 5
    
    it "ALL + inequality" $ do
      let premiumOrders = whereQ orders "ALL(items.itemPrice) > 100"
      length premiumOrders `shouldBe` 2
    
    it "ANY + inequality" $ do
      let multiItemOrders = whereQ orders "ANY(items.itemQuantity) > 1"
      length multiItemOrders `shouldBe` 1

    it "IN literals" $ do
      let multiItemOrders = whereQ orders "status in ('Pending','Cancelled')"
      length multiItemOrders `shouldBe` 2

    it "IN field" $ do
      let multiItemOrders = whereQ orders "'Laptop' in (items.itemName)"
      length multiItemOrders `shouldBe` 1

    it "BETWEEN int" $ do
      let multiItemOrders = whereQ orders "orderTotal between 100 and 200"
      length multiItemOrders `shouldBe` 1

    it "BETWEEN date" $ do
      let multiItemOrders = whereQ orders "orderDate between '2025-02-01' and '2025-02-10'"
      length multiItemOrders `shouldBe` 1

    it "ALL + BETWEEN" $ do
      let multiItemOrders = whereQ orders "all(items.itemPrice) between 20 and 130"
      length multiItemOrders `shouldBe` 1

    it "ANY + BETWEEN" $ do
      let multiItemOrders = whereQ orders "ANY(items.itemPrice) between 750 and 850"
      length multiItemOrders `shouldBe` 1
    
    it "complex query (=,<,ANY)" $ do
      let complexQuery = whereQ orders "status = 'Completed' and orderDate < '2025-02-01' and ANY(items.itemPrice) > 100"
      length complexQuery `shouldBe` 1
  
