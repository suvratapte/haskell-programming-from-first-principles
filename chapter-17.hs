 -- I dont't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Chapter_17 where

import Control.Applicative
import Data.List (elemIndex)
import Test.QuickCheck hiding (Success, Failure)
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

toList :: Foldable t => t a -> List a
toList = foldr Cons Nil

fromList :: List a -> [a]
fromList Nil        = []
fromList (Cons h t) = h : fromList t

fold :: (a -> b -> b) -> b -> List a -> b
fold _ z Nil        = z
fold f z (Cons h t) = f h (fold f z t)

concat' :: List (List a) -> List a
concat' = fold (<>) Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f l = concat' (fmap f l)

-- ZipList Applicative Exercise

-- Implement the ZipList Applicative. Use the checkers library to validate your
-- Applicative instance.

newtype ZipList' a = ZipList' (List a)
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via (List a)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ Cons x Nil
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' (Cons f fs) <*> ZipList' (Cons x xs) =
    ZipList' (Cons (f x) Nil) `mappend` (ZipList' fs <*> ZipList' xs)

-- Exercise: Variations on Either

{-
Validation has the same representation as Either, but it can be dif- ferent. The
Functor will behave the same, but the Applicative will be different. See above
for an idea of how Validation should behave. Use the checkers library.
-}

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Failure x <*> Failure y = Failure $ x <> y
  Failure x <*> _ = Failure x
  _ <*> Failure y = Failure y
  Success f <*> Success x = Success $ f x

-- Chapter Exercises

{-
Given a type that has an instance of Applicative, specialize the types of the
methods. Test your specialization in the REPL. One way to do this is to bind
aliases of the typeclass methods to more concrete types that have the type we
told you to fill in.

-- 1

pure :: a -> [a]
(<*>) ::  [ a -> b] -> [a] -> [b]

-- 2

pure :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b

-- 3

pure :: a -> (x, a)
(<*>) :: (x, (a -> b)) -> (x, a) -> (x, b)

-- 4

pure :: a -> (e -> a)
(<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
-}

{-
Write instances for the following datatypes. Confused? Write out what the type
should be. Use the checkers library to validate the instances.
-}

-- 1
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure x = Pair x x
  Pair f f' <*> Pair x x' = Pair (f x) (f' x')

-- 2
-- This should look familiar.

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  Two a f <*> Two a' x = Two (a `mappend` a') $ f x

-- 3
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  Three a b f <*> Three a' b' x =
    Three (a `mappend` a') (b `mappend` b') $ f x

-- 4
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' a f f' <*> Three' a' x x' =
    Three' (a `mappend` a') (f x) (f' x')

-- 5
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  Four a b c f <*> Four a' b' c' x =
    Four (a `mappend` a') (b `mappend` b') (c `mappend` c') $ f x

-- 6
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  Four' a1 a2 a3 f <*> Four' a1' a2' a3' x =
    Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') $ f x

-- Combinations

{-
Remember the vowels and stops exercise in the folds chapter? Write the function
to generate the possible combinations of three input lists using liftA3 from
Control.Applicative.
-}

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
