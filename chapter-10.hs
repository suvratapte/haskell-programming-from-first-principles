{-# LANGUAGE LambdaCase #-}
-- Chapter 10 exercises

-- For a later exercise
import Data.Time

-- Understanding Folds
-- 1 - b, c

-- 2
-- foldl (flip (*)) 1 [1..3]
-- foldl (flip (*)) (* 1 1) [2, 3]
-- foldl (flip (*)) (* 2 (* 1 1)) [3]
-- foldl (flip (*)) (* 3 (* 2 (* 1 1))) []
-- (* 3 (* 2 (* 1 1)))

-- 3 - c

-- 4 - a

-- 5
-- a
foldr (++) [] ["woot", "WOOT", "woot"]

-- b
foldr max [] "fear is the little death"

-- c
foldr (&&) True [False, True]

-- d -- No

-- e
foldl (flip ((++) . show)) "" [1..5]

-- f
foldr (flip const) 'a' [1..5]

-- g
foldr const '0' "t"

-- h
foldl (flip const) '0' "burritos"

-- i
foldl (flip const) 'z' ['1'..'5']

-- Exercises: Database Processing

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- 1
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\item acc ->
                        case item of
                          DbDate utcTime -> utcTime : acc
                          _ -> acc)
                     []

-- 2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\item acc ->
                          case item of
                            DbNumber num -> num : acc
                            _ -> acc)
                     []

-- 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5
avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral . sumDb) xs / (fromIntegral . length . filterDbNumber) xs

-- Scans exercises

-- 1
fibs = take 20 $ 1 : scanl (+) 1 fibs

-- 2
fibs = takeWhile (< 100) $ 1 : scanl (+) 1 fibs

-- 3
fact = scanl (*) 1 [1..]
factN = (fact !!)

-- Chapter exercises

-- Warm-up and review

-- 1
stops  = "pbtdkg"
vowels = "aeiou"

-- a
tuples = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

-- b
tuples' = [('p', y, z) | y <- vowels, z <- stops]

-- c
abaTuples as bs = [(a, b, a') | a <- as, b <- bs, a' <- as]

-- 2
-- The function returns average length of words in a string.
-- Type: String -> Int

-- 3
seekritFunc x =
  (fromIntegral . sum . map length . words) x / (fromIntegral . length . words) x

-- Rewriting functions using folds

-- 1
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\x acc -> e == x || acc) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (e ==)

-- 4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

-- 6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

-- 7
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . myMap f

-- 9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x acc ->
                            if f x acc == GT
                            then x
                            else acc)
                         (last xs)
                         (init xs)

-- 11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x acc ->
                            if f x acc == LT
                            then x
                            else acc)
                         (last xs)
                         (init xs)
