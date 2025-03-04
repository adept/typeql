{-# LANGUAGE DeriveGeneric #-}
module QueryableSpec (spec) where

import GHC.Generics
import Test.Hspec
import TypeQL.Queryable

data A = A
  { aId :: String
  } deriving (Show, Generic)

data B = B
  { bId :: String
  , bAs :: [A]
  } deriving (Show, Generic)

data CC = CC
  { cId :: String
  , cBs :: [B]
  } deriving (Show, Generic)

instance Queryable A
instance Queryable B
instance Queryable CC


spec :: Spec
spec = do
  describe "Queryable" $ do
    let a1 = A {aId = "a1"}
        a2 = A {aId = "a2"}
        b1 = B {bId = "b1", bAs = [a1, a2]}
        b2 = B {bId = "b2", bAs = [a1, a2]}
        c = CC {cId = "c1", cBs = [b1, b2]}
    
    describe "genericSelect" $ do
      it "selects a field from A" $
        genericSelect a1 "aId" `shouldBe` Just "a1"

      it "selects a non-existent field from A" $
        genericSelect a1 "foo" `shouldBe` Nothing
        
      it "selects a field from B" $
        genericSelect b1 "bId" `shouldBe` Just "b1"
        
      it "selects a field from CC" $
        genericSelect c "cId" `shouldBe` Just "c1"
    
    describe "genericSelectList" $ do
      it "selects a field list from A" $
        genericSelectList a1 ["aId"] `shouldBe` Just ["a1"]
        
      it "selects a field list from CC" $
        genericSelectList c ["cId"] `shouldBe` Just ["c1"]
        
      it "selects nested fields from CC -> B" $
        genericSelectList c ["cBs", "bId"] `shouldBe` Just ["b1", "b2"]

      it "selects nested fields from CC -> B -> A" $
        genericSelectList c ["cBs", "bAs", "aId" ] `shouldBe` Just ["a1", "a2", "a1", "a2"]


