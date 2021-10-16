-- Chapter 15 exercises

{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -w #-}

module Chapter_15 where

import Data.Monoid -- Required for some expected output code in the book.
import Test.QuickCheck (CoArbitrary, Arbitrary,  quickCheck, arbitrary, frequency)

-- Exercise: Optional Monoid

-- Write the Monoid instance for our Maybe type renamed to Optional.

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a
      => Semigroup (Optional a) where
  (<>) (Only x) (Only y) = Only $ x <> y
  (<>) (Only x) _ = Only x
  (<>) _ (Only y) = Only y
  (<>) _ _ = Nada

instance Monoid a
      => Monoid (Optional a) where
  mempty = Nada

-- Madness

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
     e
  <> "! he said "
  <> adv
  <> " as he jumped into his car "
  <> noun
  <> " and drove off with his "
  <> adj
  <> " wife."

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj =
  mconcat
    [ e
    , "! he said "
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove off with his "
    , adj
    , " wife."
    ]

-- Exercise: Maybe Another Monoid

{-
Write a Monoid instance for a Maybe type which doesn’t require a Monoid for the
contents. Reuse the Monoid law QuickCheck properties and use them to validate
the instance.

Don’t forget to write an Arbitrary instance for First'. We won’t always stub
that out explicitly for you. We suggest learning how to use the frequency
function from QuickCheck for First'’s instance.
-}

newtype First' a = First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' (Only x)) _ = First' $ Only x
  (<>) _ (First' (Only y)) = First' $ Only y
  (<>) _ _ = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (3, return . First' $ Only a)
              , (1, return $ First' Nada) ]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

semigroupAssoc :: (Semigroup a, Eq a) => a -> a -> a -> Bool
semigroupAssoc x y z = (x <> y) <> z == x <> (y <> z)

monoidLeftIdentity :: (Monoid a, Eq a) => a -> Bool
monoidLeftIdentity x = x <> mempty == x

monoidRightIdentity :: (Monoid a, Eq a) => a -> Bool
monoidRightIdentity x = mempty <> x == x

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

-- Chapter exercises

-- Semigroup exercises

{-
Given a datatype, implement the Semigroup instance. Add Semigroup constraints to
type variables where needed. Use the Semigroup class from the semigroups
library (or from base if you are on GHC 8) or write your own. When we use (<>),
we mean the infix mappend from the Semigroup typeclass.
-}

-- 1

{-
Validate all of your instances with QuickCheck. Since Semigroup’s only law is
associativity, that’s the only property you need to reuse. Keep in mind that
you’ll potentially need to import the modules for Monoid and Semigroup and to
avoid naming conflicts for the (<>) depending on your version of GHC.
-}

data Trivial = Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x <> y

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityAssoc = Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool

-- 3

data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = Two [Int] String -> Two [Int] String -> Two [Int] String -> Bool

-- 4

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc = Three [Int] String [Char] -> Three [Int] String [Char] -> Three [Int] String [Char] -> Bool

-- 5

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four w x y z) <> (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssoc = Four [Int] String [Char] [Bool] -> Four [Int] String [Char] [Bool] -> Four [Int] String [Char] [Bool] -> Bool

-- 6

{-
What it should do:
Prelude> (BoolConj True) <> (BoolConj True)
BoolConj True
Prelude> (BoolConj True) <> (BoolConj False)
BoolConj False
-}

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show, Arbitrary) via Bool

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj $ x && y

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7

{-
What it should do:
     Prelude> (BoolDisj True) <> (BoolDisj True)
     BoolDisj True
     Prelude> (BoolDisj True) <> (BoolDisj False)
     BoolDisj True
-}

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show, Arbitrary) via Bool

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj $ x || y

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8

{-
The Semigroup for Or should have the following behavior. We can think of this as
having a “sticky” Snd value where it’ll hold onto the first Snd value when and
if one is passed as an argument. This is similar to the First' Monoid you wrote
earlier.

Prelude> Fst 1 <> Snd 2
     Snd 2
     Prelude> Fst 1 <> Fst 2
     Fst 2
     Prelude> Snd 1 <> Fst 2
     Snd 1
     Prelude> Snd 1 <> Snd 2
     Snd 1

-}

data Or a b = Fst a | Snd b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency
      [ (1, return $ Fst a),
        (1, return $ Snd b) ]

instance Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  _ <> (Snd b) = Snd b
  (Fst a) <> _ = Fst a

type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

-- 9

{-
What it should do:
     Prelude> let f = Combine $ \n -> Sum (n + 1)
     Prelude> let g = Combine $ \n -> Sum (n - 1)
     Prelude> unCombine (f <> g) $ 0
     Sum {getSum = 0}
     Prelude> unCombine (f <> g) $ 1
     Sum {getSum = 2}
     Prelude> unCombine (f <> f) $ 1
     Sum {getSum = 4}
     Prelude> unCombine (g <> f) $ 1
     Sum {getSum = 2}

Hint: This function will eventually be applied to a single value of type a. But
you’ll have multiple functions that can produce a value of type b. How do we
combine multiple values so we have a single b? This one will probably be tricky!
Remember that the type of the value inside of Combine is that of a function. The
type of functions should already have an Arbitrary instance that you can reuse
for testing this instance.
-}

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \a -> f a <> g a

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

instance Show (Combine a b) where
  show _ = "Combine"

combineAssoc :: Int -> Combine Int String -> Combine Int String -> Combine Int String -> Bool
combineAssoc x f g h = unCombine ((f <> g) <> h) x == unCombine (f <> (g <> h)) x

-- 10

{-
Hint: We can do something that seems a little more specific and natural to
functions now that the input and output types are the same.
-}

newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show _ = "Comp"

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

compAssoc :: Int -> Comp Int -> Comp Int -> Comp Int -> Bool
compAssoc x f g h = unComp ((f <> g) <> h) x == unComp (f <> (g <> h)) x

-- 11

-- Look familiar?

data Validation a b = Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Success b) <> _ = (Success b)
  _ <> (Success b) = (Success b)
  (Failure a) <> (Failure a') = Failure $ a <> a'

-- Given this code:

main' = do
  let failure :: String -> Validation String Int
      failure = Failure

      success :: Int -> Validation String Int
      success = Success

  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

{-
You should get this output:

Prelude> main''
Success 1
Failure "wootblah"
Success 1
Success 2
-}

main'' :: IO ()
main'' = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck combineAssoc
  quickCheck compAssoc
