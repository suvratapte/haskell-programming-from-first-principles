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

-- Small library for Either

{-
Write each of the following functions. If more than one possible unique function
exists for the type, use common sense to determine what it should do.
-}

-- 1 -

{-
Try to eventually arrive at a solution that uses foldr, even if earlier
versions don’t use foldr.
-}

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc ->
                  case x of
                    Left a -> a : acc
                    _ -> acc)
               []

-- 2 - Same as the last one. Use foldr eventually.

rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> case x of
                   Right b -> b : acc
                   _ -> acc)
                []

-- 3

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (\x (as, bs) -> case x of
                              Left a -> (a : as, bs)
                              Right b -> (as, b : bs))
                          ([], [])

-- 4

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just . f $ b

-- 5 - This is a general catamorphism for Either values.

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fac _ (Left a) = fac a
either' _ fbc (Right b) = fbc b

-- 6 - Same as before, but use the either' function you just wrote.

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' fbc (Left _) = Nothing
eitherMaybe'' fbc (Right b) = Just . fbc $ b

-- Write your own iterate and unfoldr

-- 1

{-
Write the function myIterate using direct recursion. Compare the behavior with
the built-in iterate to gauge correctness. Do not look at the source or any
examples of iterate so that you are forced to do this yourself.
-}

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- 2

{-
Write the function myUnfoldr using direct recursion. Compare with the built-in
unfoldr to check your implementation. Again, don’t look at implementations of
unfoldr so that you figure it out yourself.
-}

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    Just (a, b) -> a : myUnfoldr f b
    Nothing -> []

-- 3

{-
Rewrite myIterate into betterIterate using myUnfoldr. A hint — we used unfoldr
to produce the same results as iterate earlier. Do this with different functions
and see if you can abstract the structure out.
-}

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

-- Finally something other than a list!

{-
Given the BinaryTree from last chapter, complete the following exercises. Here’s
that datatype again:
-}

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1 - Write unfold for BinaryTree.

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a =
  case f a of
    Just (l, b, r) -> Node (unfold f l) b (unfold f r)
    Nothing -> Leaf

-- (Not in the exercise) Code to generate a binary tree with root 5 which goes
-- to left till 0 and to the right till 10
unfold (\x ->
          if x == 5
          then Just (x - 1, x, x + 1)
          else
            if x < 5 && x > 0
            then Just (x - 1, x, 0)
            else
              if x > 5 && x < 10
              then Just (0, x, x + 1)
              else Nothing)
       5

-- 2 - Make a tree builder.

{-
Using the unfold function you’ve made for BinaryTree, write the following
function:
-}

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold (\x -> if x < n
                then Just (x + 1, x, x + 1)
                else Nothing) 0
