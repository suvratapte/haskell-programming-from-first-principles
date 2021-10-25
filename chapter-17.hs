 -- I dont't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter_17 where

import Data.List (elemIndex)
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Lookups

{-
In the following exercises you will need to use the following terms to make the
expressions typecheck:
1. pure
2. (<$>) -- or fmap
3. (<*>)

Make the following expressions typecheck.
-}

-- 1

added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3

x' :: Maybe Int
x' = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

-- 4
xs = [1, 2, 3]
ys = [4, 5, 6]

x3 :: Maybe Integer
x3 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x3 <*> y3

-- Exercise: Identity Instance

-- Write an Applicative instance for Identity.

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure x = Identity x
  Identity f <*> Identity x = Identity $ f x

-- Exercise: Constant Instance

-- Write an Applicative instance for Constant.

newtype Constant a b = Constant a
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  Constant f <*> Constant x = Constant $ f <> x

-- Exercise: Fixer Upper

{-
Given the function and values provided, use (<$>) from Functor, (<*>) and pure
from the Applicative typeclass to fill in missing bits of the broken code to
make it work.
-}

-- 1

x4 = const <$> Just "Hello" <*> pure "World"

-- 2
y4 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- List Applicative Exercise

{-
Implement the list Applicative. Writing a minimally complete Applicative
instance calls for writing the definitions of both pure and <*>. We’re going to
provide a hint as well. Use the checkers library to validate your Applicative
instance.
-}

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> Nil = Nil
  Nil <> x = x
  x <> Nil = x
  Cons x xs <> y = Cons x $ xs <> y

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = fmap f xs `mappend` (fs <*> xs)

{-
Expected result:
Prelude> let f = Cons (+1) (Cons (*2) Nil)
Prelude> let v = Cons 1 (Cons 2 Nil)
Prelude> f <*> v
Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
-}

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [ (1, return Nil),
                          (3, Cons <$> arbitrary <*> arbitrary) ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- To test with quickBatch:
-- quickBatch . applicative $ Cons ("1", "2", "3") Nil
