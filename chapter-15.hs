-- Chapter 15 exercises

import Data.Monoid -- Required for some expected output code in the book.
import Test.QuickCheck

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
