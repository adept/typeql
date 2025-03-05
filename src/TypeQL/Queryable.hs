{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeQL.Queryable (Queryable(..)) where

import GHC.Generics

class Generic a => Queryable a where
  genericSelectList :: a -> [String] -> Maybe [String]
  default genericSelectList :: (Generic a, GQueryable (Rep a)) => a -> [String] -> Maybe [String]
  genericSelectList x fields = gSelectList (from x) fields
  
  genericSelect :: a -> String -> Maybe String
  default genericSelect :: (Generic a, GQueryable (Rep a)) => a -> String -> Maybe String
  genericSelect x field = gSelect (from x) field

class GQueryable f where
  gSelectList :: f a -> [String] -> Maybe [String]
  gSelect :: f a -> String -> Maybe String

-- Unit type (empty constructor)
instance GQueryable U1 where
  gSelectList _ _ = Nothing
  gSelect _ _ = Nothing
  
-- Meta information (constructor, selector, datatype)
instance (GQueryable f, Datatype d) => GQueryable (M1 D d f) where
  gSelectList (M1 x) fields = gSelectList x fields
  gSelect (M1 x) field = gSelect x field

instance (GQueryable f, Constructor c) => GQueryable (M1 C c f) where
  gSelectList (M1 x) fields = gSelectList x fields
  gSelect (M1 x) field = gSelect x field

instance (GQueryable f, Selector s) => GQueryable (M1 S s f) where
  gSelectList (M1 x) [] = Nothing
  gSelectList m@(M1 x) (field:rest) 
    | selectorName == field = case rest of
        [] -> gSelectList x []  -- End of path, get all values
        _  -> gSelectList x rest  -- Continue with rest of path
    | otherwise = Nothing
    where selectorName = selName m
    
  gSelect m@(M1 x) field 
    | selectorName == field = case gSelect x "" of
        Just v -> Just v
        Nothing -> Nothing
    | otherwise = Nothing
    where selectorName = selName m

-- Special instance for String fields
instance {-# OVERLAPPING #-} GQueryable (K1 R String) where
  gSelectList (K1 x) [] = Just [x]
  gSelectList _ _ = Nothing
  
  gSelect (K1 x) "" = Just x
  gSelect _ _ = Nothing

instance {-# OVERLAPPABLE #-} Show a => GQueryable (K1 R a) where
  gSelectList (K1 x) [] = Just [show x]
  gSelectList _ _ = Nothing
  
  gSelect (K1 x) "" = Just (show x)
  gSelect _ _ = Nothing
  
-- Catch-all for other types
instance {-# OVERLAPPABLE #-} GQueryable (K1 i a) where
  gSelectList _ _ = Nothing
  gSelect _ _ = Nothing
  
-- Special instance for list handling (only for queryable record types)
instance {-# OVERLAPPING #-} (Generic a, Queryable a, a ~ a) => GQueryable (K1 R [a]) where
  gSelectList (K1 xs) [] = Nothing  -- No direct field name to match, get inner values
  gSelectList (K1 xs) (field:rest) = 
    let results = map (\x -> genericSelectList x (field:rest)) xs
        filtered = [vals | Just vals <- results]
    in if null filtered 
       then Nothing
       else Just (concat filtered)
  
  gSelect (K1 _) _ = Nothing  -- Select only works for single values, not lists

instance (GQueryable a, GQueryable b) => GQueryable (a :*: b) where
  gSelectList (a :*: b) fields = case (gSelectList a fields, gSelectList b fields) of
    (Just xs, Just ys) -> Just (xs ++ ys)
    (Just xs, Nothing) -> Just xs
    (Nothing, Just ys) -> Just ys
    (Nothing, Nothing) -> Nothing
    
  gSelect (a :*: b) field = case gSelect a field of
    Just x -> Just x
    Nothing -> gSelect b field
    
-- Sum (alternatives)
instance (GQueryable a, GQueryable b) => GQueryable (a :+: b) where
  gSelectList (L1 x) fields = gSelectList x fields
  gSelectList (R1 x) fields = gSelectList x fields
  
  gSelect (L1 x) field = gSelect x field
  gSelect (R1 x) field = gSelect x field
