 -- I don't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}

module Chapter_20 where

import Data.Monoid

-- Exercises: Library Functions

{-
Implement the functions in terms of foldMap or foldr from Foldable, then try
them out with multiple types that have Foldable instances.
-}

-- 1
-- This and the next one are nicer with foldMap, but foldr is fine too.

sumFoldMap :: (Foldable t, Num a) => t a -> a
sumFoldMap = getSum . foldMap Sum

sumFoldr :: (Foldable t, Num a) => t a -> a
sumFoldr = foldr (+) 0

-- 2
productFoldMap :: (Foldable t, Num a) => t a -> a
productFoldMap = getProduct . foldMap Product

productFoldr :: (Foldable t, Num a) => t a -> a
productFoldr = foldr (*) 1

--3
elemFoldMap :: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldMap e = getAny . foldMap (\e' ->
                                    if e == e'
                                    then Any True
                                    else Any False)

elemFoldr :: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldr e = foldr (\e' acc -> (e == e') || acc) False

-- 4
minimumFoldr :: (Foldable t, Ord a) => t a -> Maybe a
minimumFoldr = foldr (\e acc ->
                        case acc of
                          Nothing -> Just e
                          Just e' -> Just $ min e e')
                     Nothing

-- TODO: Try this with `foldMap` as well: Try writing a monoid for `Num a => a`
-- which will return the minimum element on a1 <> a2.

-- 5
maximumFoldr :: (Foldable t, Ord a) => t a -> Maybe a
maximumFoldr = foldr (\e acc ->
                        case acc of
                          Nothing -> Just e
                          Just e' -> Just $ max e e')
                     Nothing

-- 6
nullFoldMap :: (Foldable t) => t a -> Bool
nullFoldMap = getAll . foldMap (const (All False))

nullFoldr :: (Foldable t) => t a -> Bool
nullFoldr = foldr (\_ _ -> False) True

-- 7
lengthFoldMap :: (Foldable t) => t a -> Int
lengthFoldMap = getSum . foldMap (const (Sum 1))

lengthFoldr :: (Foldable t) => t a -> Int
lengthFoldr = foldr (\_ acc -> acc + 1) 0

-- 8
-- Some say this is all Foldable amounts to.
toListFoldMap :: (Foldable t) => t a -> [a]
toListFoldMap = foldMap (: [])

toListFoldr :: (Foldable t) => t a -> [a]
toListFoldr = foldr (:) []

-- 9
-- Hint: use foldMap.
-- | Combine the elements
-- of a structure using a monoid.

foldFoldMap :: (Foldable t, Monoid m) => t m -> m
foldFoldMap = foldMap id

foldFoldr :: (Foldable t, Monoid m) => t m -> m
foldFoldr = foldr (<>) mempty

-- 10
-- Define foldMap in terms of foldr.
foldMapFoldr :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMapFoldr f = foldr ((<>) . f) mempty

-- Reads better as: foldr (\e acc -> f e <> acc) mempty

-- Chapter Exercises

-- Write Foldable instances for the following datatypes.

-- 1
data Constant a b = Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

-- 2
data Two a b = Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- 3
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- 4
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

data Four' a b = Four' a b b b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

-- Thinking cap time. Write a filter function for Foldable types using foldMap.

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)
