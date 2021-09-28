-- Chapter 12 exercises

import Data.Maybe

-- Chapter exercises

-- Determine the kinds

-- 1 -

{-
Given
id :: a -> a
What is the kind of a?
-}

-- Ans: *

-- 2

{-
r :: a -> f a
What are the kinds of a and f?
-}

-- Ans:
-- a :: *
-- f :: * -> *

-- String processing

-- 1

{-
WritearecursivefunctionnamedreplaceThewhichtakesatext/string, breaks it into
words and replaces each instance of “the” with “a”.  It’s intended only to
replace exactly the word “the”. notThe is a suggested helper function for
accomplishing this.
-}

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe "The" = Nothing
notThe str = Just str

replaceThe :: String -> String
replaceThe = unwords . go . words
  where go [] = []
        go (x : xs) = fromMaybe "a" (notThe x) : go xs

-- 2

{-
Write a recursive function that takes a text/string, breaks it into words, and
counts the number of instances of ”the” followed by a vowel-initial word.
-}

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where go [] = 0
        go ("the" : xs@( x@(v : _) : _)) =
          if v `elem` "aeiouAEIOU" then 1 else 0 + go xs
        go ("The" : xs@( x@(v : _) : _)) =
          if v `elem` "aeiouAEIOU" then 1 else 0 + go xs
        go (x : xs) = go xs

-- 3

{-
Return the number of letters that are vowels in a word.
Hint: it’s helpful to break this into steps. Add any helper functions
necessary to achieve your objectives.
a) Test for vowelhood
b) Return the vowels of a string
c) Count the number of elements returned
-}

countVowels :: String -> Integer
countVowels [] = 0
countVowels (x:xs)
  | x `elem` "aeiouAEIOU" = 1 + countVowels xs
  | otherwise = countVowels xs

-- Validate the word

{-
Use the Maybe type to write a function that counts the number of vowels in a
string and the number of consonants. If the number of vowels exceeds the number
of consonants, the function returns Nothing. In many human languages, vowels
rarely exceed the number of consonants so when they do, it may indicate the
input isn’t a word (that is, a valid input to your dataset):
-}

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str =
  let stripped = filter (/= ' ') str
      countOfVowels = countVowels stripped
      countOfConsonants = fromIntegral (length stripped) - countOfVowels
  in
    if countOfVowels <= countOfConsonants
    then Just . Word' $ str
    else Nothing

-- It’s only Natural

{-
You’ll be presented with a datatype to represent the natural numbers. The only
values representable with the naturals are whole numbers from zero to
infinity. Your task will be to implement functions to convert Naturals to
Integers and Integers to Naturals. The conversion from Naturals to Integers
won’t return Maybe because Integer is a strict superset of Natural. Any Natural
can be represented by an Integer, but the same is not true of any
Integer. Negative numbers are not valid natural numbers.
-}

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat num
  | num < 0 = Nothing
  | otherwise = Just $ go num
  where go 0 = Zero
        go num = Succ (go $ num - 1)

-- Small library for Maybe

-- 1 - Simple boolean checks for Maybe values.
isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2

{-
The following is the Maybe catamorphism. You can turn a Maybe value into
anything else with this.
-}

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee backup _ _ = backup

-- 3 - In case you just want to provide a fallback value.

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

-- 4 - Converting between List and Maybe.

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

-- 5 - For when we want to drop the Nothing values from our list.

catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

-- 6 - You’ll see this called “sequence” later.

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe mxs =
  if all isJust mxs
  then Just . catMaybes $ mxs
  else Nothing
