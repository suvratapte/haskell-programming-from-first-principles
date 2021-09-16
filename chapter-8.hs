-- Chapter 8 exercises

import Data.List (intercalate)

-- Intermission
-- applyTimes 5 (+1) 5
-- (+1) (applyTimes 4 (+1) 5)
-- (+1) (+1) (applyTimes 3 (+1) 5)
-- (+1) (+1) (+1) (applyTimes 2 (+1) 5)
-- (+1) (+1) (+1) (+1) (applyTimes 1 (+1) 5)
-- (+1) (+1) (+1) (+1) (+1) (applyTimes 0 (+1) 5)
-- (+1) (+1) (+1) (+1) (+1) 5
-- (+1) (+1) (+1) (+1) 6
-- (+1) (+1) (+1) 7
-- (+1) (+1) 8
-- (+1) 9
-- 10

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- f 6
-- f 5 + f 4
-- f 4 + f 3 + f 4
-- f 3 + f 2 + f 3 + f 4
-- f 2 + f 1 + f 2 + f 3 + f 4
-- f 1 + f 0 + f 1 + f 2 + f 3 + f 4
-- 1 + 0 + 1 + f 2 + f 3 + f 4
-- 1 + 0 + 1 + f 1 + f 0 + f 3 + f 4
-- 1 + 0 + 1 + 1 + 0 + f 3 + f 4
-- 1 + 0 + 1 + 1 + 0 + f 2 + f 1 + f 4
-- 1 + 0 + 1 + 1 + 0 + f 1 + f 0 + f 1 + f 4
-- 1 + 0 + 1 + 1 + 0 + 1 + 0 + 1 + f 4
-- 1 + 0 + 1 + 1 + 0 + 1 + 0 + 1 + f 3 + f 2
-- 1 + 0 + 1 + 1 + 0 + 1 + 0 + 1 + f 2 + f 1 + f 2
-- 1 + 0 + 1 + 1 + 0 + 1 + 0 + 1 + f 1 + f 0 + f 1 + f 2
-- 1 + 0 + 1 + 1 + 0 + 1 + 0 + 1 + 1 + 0 + 1 + f 2
-- 1 + 0 + 1 + 1 + 0 + 1 + 0 + 1 + 1 + 0 + 1 + f 1 + f 0
-- 1 + 0 + 1 + 1 + 0 + 1 + 0 + 1 + 1 + 0 + 1 + 1 + 0
-- 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
-- 8

-- Chapter exercises

-- Review of types

-- 1 - d
-- 2 - b
-- 3 - d
-- 4 - b

-- Reviewing currying

-- 1 "woops mrow woohoo!"
-- 2 "1 mrow haha"
-- 3 "woops mrow 2 mrow haha"
-- 4 "woops mrow blue mrow haha"
-- 5 "pink mrow haha mrow green mrow woops mrow blue"
-- 6 "are mrow Puga mrow awesome"

-- Recursion

-- 1
-- dividedBy 15 2
-- 15 2 0
-- 13 2 1
-- 11 2 2
-- 9 2 3
-- 7 2 4
-- 5 2 5
-- 3 2 6
-- 1 2 7
-- (7, 1)

-- 2
sumUptoN :: (Num a, Eq a) => a -> a
sumUptoN 0 = 0
sumUptoN 1 = 1
sumUptoN n = n + sumUptoN (n - 1)

-- 3
multBy :: (Num a, Eq a) => a -> a -> a
multBy x 0 = 0
multBy x y = x + multBy x (y - 1)

-- Fixing divideBy

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

data DividedResult = Result Integer | DividedByZero deriving Show

dividedByFixed :: Integer -> Integer -> DividedResult
dividedByFixed num denom
  | denom == 0 = DividedByZero
  | num < 0 && denom < 0 = Result . fst $ dividedBy (abs num) (abs denom)
  | num < 0 || denom < 0 = Result . negate . fst $ dividedBy (abs num) (abs denom)
  | otherwise = Result . fst $ dividedBy num denom

-- McCarthy 91 function

mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11

-- Numbers into words

digitToWord :: Int -> String
digitToWord n = digitWords !! n
  where digitWords = ["zero", "one", "two", "three", "four"
                     , "five", "six", "seven", "eight", "nine"]

digits :: Int -> [Int]
digits n = go n []
  where go n xs
          | n == 0 = xs
          | otherwise = go (div n 10) (mod n 10 : xs)

wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord (digits n))
