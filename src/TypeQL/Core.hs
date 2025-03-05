{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module TypeQL.Core
  ( whereQ
  ) where

import Text.Read (readMaybe)

import TypeQL.AST
import TypeQL.Queryable

-- | Main query function that applies a query string to filter a list
whereQ :: Queryable a => [a] -> String -> [a]
whereQ items queryString =
  case parseExpr queryString of
    Left err -> error $ "Query parse error: " ++ show err
    Right expr -> filter (evaluateToBool expr) items
  where
    evaluateToBool expr item =
      case eval item expr of
        Right (EBool b) -> b
        Right _ -> error "Query did not evaluate to a boolean"
        Left err -> error $ "Query evaluation error: " ++ err

-- | Different types of values that expressions can evaluate to
data ExprValue
  = EString String
  | EList [String]
  | EBool Bool
  deriving (Show, Eq)

-- | Try to convert an expression value to a boolean
asBool :: ExprValue -> Either String Bool
asBool (EBool b) = Right b
asBool (EString s)
  | s == "true" || s == "True" = Right True
  | s == "false" || s == "False" = Right False
  | otherwise = Left $ "Cannot convert string to boolean: " ++ s
asBool (EList []) = Right False
asBool (EList _) = Right True

-- | Try to convert an expression value to a string
asString :: ExprValue -> Either String String
asString (EString s) = Right $ strip s
asString (EBool True) = Right "true"
asString (EBool False) = Right "false"
asString (EList _) = Left "Cannot convert list to string"

-- | Helper function to strip quotes from strings
strip :: String -> String
strip s = case s of
  ('"':xs) | last xs == '"' -> init xs
  ('\'':xs) | last xs == '\'' -> init xs
  _ -> s

-- | Try to convert an expression value to a list
asList :: ExprValue -> Either String [String]
asList (EList xs) = Right $ map strip xs
asList (EString s) = Right [strip s]
asList (EBool True) = Right ["true"]
asList (EBool False) = Right ["false"]

-- | Compare two expression values with the given comparison operator
compareValues :: ExprValue -> ExprValue -> Comparison -> Either String Bool
compareValues left right comparison = do
  -- Try numeric comparison first
  case (readMaybe =<< asString' left, readMaybe =<< asString' right) of
    (Just leftNum, Just rightNum) -> Right $ compareF comparison (leftNum :: Double) rightNum
    _ -> do
      -- Fall back to string comparison
      leftStr <- asString left
      rightStr <- asString right
      Right $ compareF comparison leftStr rightStr
  where
    asString' :: ExprValue -> Maybe String
    asString' val = either (const Nothing) Just (asString val)

-- | Map comparison operators to their function implementations
compareF :: Ord a => Comparison -> (a -> a -> Bool)
compareF Equal = (==)
compareF NotEqual = (/=)
compareF GreaterThan = (>)
compareF LessThan = (<)
compareF GreaterThanOrEqual = (>=)
compareF LessThanOrEqual = (<=)

-- convert (x <= y) into (y >= x)
reflect :: Comparison -> Comparison
reflect Equal = Equal
reflect NotEqual = NotEqual
reflect GreaterThan = LessThan
reflect LessThan = GreaterThan
reflect GreaterThanOrEqual = LessThanOrEqual
reflect LessThanOrEqual = GreaterThanOrEqual

-- | Evaluate an expression against a queryable value
eval :: Queryable a => a -> Expr -> Either String ExprValue
eval value expr = case expr of
  -- Basic expressions
  Literal s -> Right $ EString s
  LiteralList strs -> Right $ EList strs
  FieldRef field -> maybe (Left $ "Field not found: " ++ field) (Right . EString) $ genericSelect value field
  FieldPath path -> maybe (Left $ "Path not found: " ++ show path) (Right . EList) $ genericSelectList value path
  
  -- Boolean operations
  And left right -> do
    leftResult <- eval value left >>= asBool
    rightResult <- eval value right >>= asBool
    Right $ EBool (leftResult && rightResult)
    
  Or left right -> do
    leftResult <- eval value left >>= asBool
    rightResult <- eval value right >>= asBool
    Right $ EBool (leftResult || rightResult)
    
  Not subExpr -> do
    result <- eval value subExpr >>= asBool
    Right $ EBool (not result)
  
  -- Quantifier operators in comparison context
  Compare cmp (Any subExpr) right -> evalAnyComparison value cmp subExpr right
  Compare cmp left (Any subExpr) -> evalAnyComparison value (reflect cmp) subExpr left
  Compare cmp (All subExpr) right -> evalAllComparison value cmp subExpr right
  Compare cmp left (All subExpr) -> evalAllComparison value (reflect cmp) subExpr left
  
  -- Regular comparison
  Compare cmp left right -> do
    leftResult <- eval value left
    rightResult <- eval value right
    compareResult <- compareValues leftResult rightResult cmp
    Right $ EBool compareResult
    
  Between (Any subExpr) low high -> do
    lowResult <- evalAnyComparison value GreaterThanOrEqual subExpr low >>= asBool
    highResult <- evalAnyComparison value LessThanOrEqual subExpr high >>= asBool
    Right $ EBool (lowResult && highResult)

  Between (All subExpr) low high -> do
    lowResult <- evalAllComparison value GreaterThanOrEqual subExpr low >>= asBool
    highResult <- evalAllComparison value LessThanOrEqual subExpr high >>= asBool
    Right $ EBool (lowResult && highResult)
    
  -- Between operation
  Between subExpr low high -> do
    subResult <- eval value subExpr
    lowResult <- eval value low
    highResult <- eval value high
    greaterOrEqual <- compareValues subResult lowResult GreaterThanOrEqual
    lessOrEqual <- compareValues subResult highResult LessThanOrEqual
    Right $ EBool (greaterOrEqual && lessOrEqual)

  -- In operation
  In target collection -> do
    targetResult <- eval value target
    collectionResult <- eval value collection
    collectionList <- asList collectionResult
    targetStr <- asString targetResult
    Right $ EBool (targetStr `elem` collectionList)
    
  -- Standalone ANY operator
  Any subExpr -> do
    result <- eval value subExpr
    resultList <- asList result
    Right $ EBool (any fromString resultList)
    
  -- Standalone ALL operator
  All subExpr -> do
    result <- eval value subExpr
    resultList <- asList result
    Right $ EBool (not (null resultList) && all fromString resultList)

-- | Evaluate a comparison where the left side is ANY(...)
evalAnyComparison :: Queryable a => a -> Comparison -> Expr -> Expr -> Either String ExprValue
evalAnyComparison value cmp subExpr right = do
  subResult <- eval value subExpr
  rightResult <- eval value right
  subList <- asList subResult
  
  if null subList
    then Right $ EBool False
    else do
      compareResults <- mapM (\item -> compareValues (EString item) rightResult cmp) subList
      Right $ EBool (any id compareResults)

-- | Evaluate a comparison where the left side is ALL(...)
evalAllComparison :: Queryable a => a -> Comparison -> Expr -> Expr -> Either String ExprValue
evalAllComparison value cmp subExpr right = do
  subResult <- eval value subExpr
  rightResult <- eval value right
  subList <- asList subResult
  
  if null subList
    then Right $ EBool False
    else do
      compareResults <- mapM (\item -> compareValues (EString item) rightResult cmp) subList
      Right $ EBool (all id compareResults)

