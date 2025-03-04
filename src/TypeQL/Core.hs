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

import Data.Maybe (fromMaybe, isJust, catMaybes)
import GHC.Generics (Generic, Rep)
import Text.Read (readMaybe)
import Data.List (isInfixOf)

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
  | ENull
  deriving (Show, Eq)

-- | Try to convert an expression value to a boolean
toBool :: ExprValue -> Either String Bool
toBool (EBool b) = Right b
toBool (EString s)
  | s == "true" || s == "1" = Right True
  | s == "false" || s == "0" || s == "" = Right False
  | otherwise = Left $ "Cannot convert string to boolean: " ++ s
toBool (EList []) = Right False
toBool (EList _) = Right True
toBool ENull = Right False

-- | Try to convert an expression value to a string
toString :: ExprValue -> Either String String
toString (EString s) = Right s
toString (EBool True) = Right "true"
toString (EBool False) = Right "false"
toString (EList _) = Left "Cannot convert list to string"
toString ENull = Right ""

-- | Try to convert an expression value to a list
toList :: ExprValue -> Either String [String]
toList (EList xs) = Right xs
toList (EString s) = Right [s]
toList (EBool True) = Right ["true"]
toList (EBool False) = Right ["false"]
toList ENull = Right []

-- | Compare two expression values with the given comparison operator
compareValues :: ExprValue -> ExprValue -> Comparison -> Either String Bool
compareValues left right comparison = do
  -- Try numeric comparison first
  case (readMaybe =<< toString' left, readMaybe =<< toString' right) of
    (Just leftNum, Just rightNum) -> Right $ compareF comparison (leftNum :: Double) rightNum
    _ -> do
      -- Fall back to string comparison
      leftStr <- toString left
      rightStr <- toString right
      Right $ compareF comparison leftStr rightStr
  where
    toString' :: ExprValue -> Maybe String
    toString' val = either (const Nothing) Just (toString val)

-- | Map comparison operators to their function implementations
compareF :: Ord a => Comparison -> (a -> a -> Bool)
compareF Equal = (==)
compareF NotEqual = (/=)
compareF GreaterThan = (>)
compareF LessThan = (<)
compareF GreaterThanOrEqual = (>=)
compareF LessThanOrEqual = (<=)

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
    leftResult <- eval value left >>= toBool
    rightResult <- eval value right >>= toBool
    Right $ EBool (leftResult && rightResult)
    
  Or left right -> do
    leftResult <- eval value left >>= toBool
    rightResult <- eval value right >>= toBool
    Right $ EBool (leftResult || rightResult)
    
  Not subExpr -> do
    result <- eval value subExpr >>= toBool
    Right $ EBool (not result)
  
  -- Quantifier operators in comparison context
  Compare cmp (Any subExpr) right -> evalAnyComparison value cmp subExpr right
  Compare cmp left (Any subExpr) -> evalComparisonAny value cmp left subExpr
  Compare cmp (All subExpr) right -> evalAllComparison value cmp subExpr right
  Compare cmp left (All subExpr) -> evalComparisonAll value cmp left subExpr
  
  -- Regular comparison
  Compare cmp left right -> do
    leftResult <- eval value left
    rightResult <- eval value right
    compareResult <- compareValues leftResult rightResult cmp
    Right $ EBool compareResult
    
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
    collectionList <- toList collectionResult
    targetStr <- toString targetResult
    Right $ EBool (targetStr `elem` collectionList)
    
  -- Standalone ANY operator
  Any subExpr -> do
    result <- eval value subExpr
    resultList <- toList result
    Right $ EBool (any nonEmpty resultList)
    
  -- Standalone ALL operator
  All subExpr -> do
    result <- eval value subExpr
    resultList <- toList result
    Right $ EBool (not (null resultList) && all nonEmpty resultList)
    
  where
    nonEmpty s = s /= "" && s /= "false" && s /= "0"

-- | Evaluate a comparison where the left side is ANY(...)
evalAnyComparison :: Queryable a => a -> Comparison -> Expr -> Expr -> Either String ExprValue
evalAnyComparison value cmp subExpr right = do
  subResult <- eval value subExpr
  rightResult <- eval value right
  subList <- toList subResult
  
  if null subList
    then Right $ EBool False
    else do
      compareResults <- mapM (\item -> compareValues (EString item) rightResult cmp) subList
      Right $ EBool (any id compareResults)

-- | Evaluate a comparison where the right side is ANY(...)
evalComparisonAny :: Queryable a => a -> Comparison -> Expr -> Expr -> Either String ExprValue
evalComparisonAny value cmp left subExpr = do
  leftResult <- eval value left
  subResult <- eval value subExpr
  subList <- toList subResult
  
  if null subList
    then Right $ EBool False
    else do
      compareResults <- mapM (\item -> compareValues leftResult (EString item) cmp) subList
      Right $ EBool (any id compareResults)

-- | Evaluate a comparison where the left side is ALL(...)
evalAllComparison :: Queryable a => a -> Comparison -> Expr -> Expr -> Either String ExprValue
evalAllComparison value cmp subExpr right = do
  subResult <- eval value subExpr
  rightResult <- eval value right
  subList <- toList subResult
  
  if null subList
    then Right $ EBool False
    else do
      compareResults <- mapM (\item -> compareValues (EString item) rightResult cmp) subList
      Right $ EBool (all id compareResults)

-- | Evaluate a comparison where the right side is ALL(...)
evalComparisonAll :: Queryable a => a -> Comparison -> Expr -> Expr -> Either String ExprValue
evalComparisonAll value cmp left subExpr = do
  leftResult <- eval value left
  subResult <- eval value subExpr
  subList <- toList subResult
  
  if null subList
    then Right $ EBool False
    else do
      compareResults <- mapM (\item -> compareValues leftResult (EString item) cmp) subList
      Right $ EBool (all id compareResults)
