{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import Test.Hspec

import TypeQL.AST

spec :: Spec
spec = describe "TypeQL Expression Parser" $ do
  describe "Basic Literal Parsing" $ do
    it "parses numeric literals" $ do
      parseExpr "42" `shouldBe` Right (Literal "42")
      parseExpr "42.5" `shouldBe` Right (Literal "42.5")
      parseExpr "-17.3" `shouldBe` Right (Literal "-17.3")

    it "parses boolean literals" $ do
      parseExpr "true" `shouldBe` Right (Literal "True")
      parseExpr "false" `shouldBe` Right (Literal "False")

    it "parses string literals with double quotes" $ do
      parseExpr "name = \"John\"" `shouldBe` 
        Right (Compare Equal (FieldRef "name") (Literal "John"))

    it "parses string literals with single quotes" $ do
      parseExpr "name = 'Jane'" `shouldBe` 
        Right (Compare Equal (FieldRef "name") (Literal "Jane"))

    it "parses string literals with escaped quotes" $ do
      parseExpr "message = 'It\\'s a test'" `shouldBe` 
        Right (Compare Equal (FieldRef "message") (Literal "It's a test"))
      parseExpr "message = \"Say \\\"hello\\\"\"" `shouldBe` 
        Right (Compare Equal (FieldRef "message") (Literal "Say \"hello\""))

    it "parses string literals with escape sequences" $ do
      parseExpr "text = 'New\\nline'" `shouldBe` 
        Right (Compare Equal (FieldRef "text") (Literal "New\nline"))
      parseExpr "text = \"Tab\\tcharacter\"" `shouldBe` 
        Right (Compare Equal (FieldRef "text") (Literal "Tab\tcharacter"))

  describe "Whitespace Handling" $ do
    it "handles whitespace in comparisons" $ do
      parseExpr "age > 18" `shouldBe` 
        Right (Compare GreaterThan (FieldRef "age") (Literal "18"))
      parseExpr "age  >  18" `shouldBe` 
        Right (Compare GreaterThan (FieldRef "age") (Literal "18"))
      parseExpr " age > 18 " `shouldBe` 
        Right (Compare GreaterThan (FieldRef "age") (Literal "18"))

  describe "Comparison Operators" $ do
    it "parses equality comparisons" $ do
      parseExpr "age = 30" `shouldBe` 
        Right (Compare Equal (FieldRef "age") (Literal "30"))
      parseExpr "name != 'Test'" `shouldBe` 
        Right (Compare NotEqual (FieldRef "name") (Literal "Test"))

    it "parses inequality comparisons" $ do
      parseExpr "score > 90" `shouldBe` 
        Right (Compare GreaterThan (FieldRef "score") (Literal "90"))
      parseExpr "amount < 100.5" `shouldBe` 
        Right (Compare LessThan (FieldRef "amount") (Literal "100.5"))

    it "parses GEQ/LEQ comparisons" $ do
      parseExpr "score >= 90" `shouldBe` 
        Right (Compare GreaterThanOrEqual (FieldRef "score") (Literal "90"))
      parseExpr "amount <= 100.5" `shouldBe` 
        Right (Compare LessThanOrEqual (FieldRef "amount") (Literal "100.5"))

  describe "Logical Operators" $ do
    it "parses simple AND conditions" $ do
      parseExpr "age > 18 and status = 'active'" `shouldBe` 
        Right (And 
          (Compare GreaterThan (FieldRef "age") (Literal "18"))
          (Compare Equal (FieldRef "status") (Literal "active")))
      parseExpr "age  >  18 and   status = 'active'" `shouldBe` 
        Right (And 
          (Compare GreaterThan (FieldRef "age") (Literal "18"))
          (Compare Equal (FieldRef "status") (Literal "active")))

    it "parses simple OR conditions" $ do
      parseExpr "role = 'admin' or status = 'active'" `shouldBe` 
        Right (Or 
          (Compare Equal (FieldRef "role") (Literal "admin"))
          (Compare Equal (FieldRef "status") (Literal "active")))

    it "parses NOT conditions" $ do
      parseExpr "not active = 12" `shouldBe` 
        Right (Not (Compare Equal (FieldRef "active") (Literal "12")))

  describe "ANY/ALL Expressions" $ do
    it "parses ANY on the left" $ do
      parseExpr "ANY(users.role) = 'admin'" `shouldBe` 
        Right (Compare Equal (Any (FieldPath ["users", "role"])) (Literal "admin"))

    it "parses ANY on the right" $ do
      parseExpr "status = ANY(users.role)" `shouldBe` 
        Right (Compare Equal (FieldRef "status") (Any (FieldPath ["users", "role"])))

    it "parses ALL on the left" $ do
      parseExpr "ALL(orders.status) != 'pending'" `shouldBe` 
        Right (Compare NotEqual (All (FieldPath ["orders","status"])) (Literal "pending"))

    it "parses ALL on the right" $ do
      parseExpr "status = ALL(orders.status)" `shouldBe` 
        Right (Compare Equal (FieldRef "status") (All (FieldPath ["orders","status"])))

  describe "Field Path Expressions" $ do
    it "parses nested field paths" $ do
      parseExpr "user.profile.email = 'test@example.com'" `shouldBe` 
        Right (Compare Equal (FieldPath ["user", "profile", "email"]) (Literal "test@example.com"))
      
    it "handles single-level field references" $ do
      parseExpr "name" `shouldBe` Right (FieldRef "name")
      
    it "handles multi-level field paths in expressions" $ do
      parseExpr "user.address.zipCode > 90000" `shouldBe`
        Right (Compare GreaterThan (FieldPath ["user", "address", "zipCode"]) (Literal "90000"))

  describe "Literal List Expressions" $ do
    it "parses lists of literals" $ do
      parseExpr "(1, 2, 3, 4)" `shouldBe` 
        Right (LiteralList ["1", "2", "3", "4"])
        
    it "parses lists with string literals" $ do
      parseExpr "('red', 'green', 'blue')" `shouldBe`
        Right (LiteralList ["red", "green", "blue"])
        
    it "handles mixed literal types in lists" $ do
      parseExpr "('test', 42, -1.5)" `shouldBe`
        Right (LiteralList ["test", "42", "-1.5"])

  describe "Between Expressions" $ do
    it "parses simple between expressions" $ do
      parseExpr "age between 18 and 65" `shouldBe`
        Right (Between (FieldRef "age") (Literal "18") (Literal "65"))
        
    it "parses between with field paths" $ do
      parseExpr "user.stats.score between 100 and 500" `shouldBe`
        Right (Between (FieldPath ["user", "stats", "score"]) (Literal "100") (Literal "500"))
        
    it "handles complex expressions in between" $ do
      parseExpr "age between (min_age) and (max_age)" `shouldBe`
        Right (Between 
           (FieldRef "age")
           (FieldRef "min_age")
           (FieldRef "max_age"))

  describe "In Expressions" $ do
    it "parses basic IN with literal list" $ do
      parseExpr "status in ('active', 'pending', 'completed')" `shouldBe`
        Right (In (FieldRef "status") (LiteralList ["active", "pending", "completed"]))
        
    it "parses IN with field paths" $ do
      parseExpr "user.role in ('admin', 'moderator')" `shouldBe`
        Right (In (FieldPath ["user", "role"]) (LiteralList ["admin", "moderator"]))
        
    it "handles IN with a field reference as the right operand" $ do
      parseExpr "user.id in allowed_users" `shouldBe`
        Right (In (FieldPath ["user", "id"]) (FieldRef "allowed_users"))

    it "handles IN with literal and field path" $ do
      parseExpr "'Laptop' in (items.itemName)" `shouldBe`
        Right (In (Literal "Laptop") (FieldPath ["items", "itemName"]))

  describe "Complex Expressions" $ do
    it "parses nested and complex boolean expressions" $ do
      parseExpr "(age > 18 and status = 'active') or role = 'admin'" `shouldBe` 
        Right (Or 
          (And 
            (Compare GreaterThan (FieldRef "age") (Literal "18"))
            (Compare Equal (FieldRef "status") (Literal "active")))
          (Compare Equal (FieldRef "role") (Literal "admin")))
