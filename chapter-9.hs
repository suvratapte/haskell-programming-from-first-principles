-- Chapter 9 exercises

import Data.Bool
import Data.Char

-- EnumFromTo

eft :: (Ord a, Eq a, Enum a) => a -> a -> [a]
eft from to
  | from > to = []
  | from == to = [to]
  | otherwise = from : eft (succ from) to

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

-- Thy Fearful Symmetry

-- 3 (3 is the generalization of 1 and 2)
splice :: (Eq a) => [a] -> a -> [[a]]
splice xs e = go xs []
  where go xs xss
          | null xs = xss
          | otherwise = go (drop 1 (dropWhile (/= e) xs))
                           (xss ++ [takeWhile (/= e) xs])
-- 1
myWords :: String -> [String]
myWords str = splice str ' '

-- 2
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines str = splice str '\n'

-- myLines sentences
shouldEqual =
       [ "Tyger Tyger, burning bright"
       , "In the forests of the night"
       , "What immortal hand or eye"
       , "Could frame thy fearful symmetry?"
       ]
-- The main function here is a small test -- to ensure you've written your function -- correctly.
main :: IO ()
main = print $
       "Are they equal? " ++ show (myLines sentences == shouldEqual)

-- Comprehend Thy Lists

-- 1 - Return the even squares
-- 2 - Pairs of squares less than 50 and greater than 50
-- 3 - First five elements of the above list

-- Square Cube
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- 1
tuples = [(x, y) | x <- mySqr, y <- myCube]

-- 2
tuples' = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50]

-- 3
count = length tuples'

-- Bottom Madness

-- Will it blow up?
-- 1 - Yes
-- 2 - No
-- 3 - Yes
-- 4 - No
-- 5 - Yes
-- 6 - No
-- 7 - Yes
-- 8 - No
-- 9 - No
-- 10 - Yes

-- Intermission: Is it in normal form?
-- 1 - NF
-- 2 - WHNF
-- 3 - Neither
-- 4 - Neither
-- 5 - WHNF
-- 6 - Neither
-- 7 - WHNF

-- More Bottoms
-- 1 - No
-- 2 - Yes
-- 3 - No

-- 4
-- Type: [Char] -> [Bool]
-- Takes a string and returns a list of Bools (same length as the string) the
-- Bools are True if the the character at the correspdonding index in the string
-- is a vowel; False otherwise.

-- 5
-- a - List of squares of numbers from 1 to 10
-- b - [1, 10, 20]
-- c - [15, 15, 15]

-- 6
map (\x -> bool x (-x) (x == 3)) [1..10]

-- Filtering
-- 1
filter (\x -> mod x 3 == 0) [1..30]

-- 2
length . filter (\x -> mod x 3 == 0) $ [1..30]

-- 3
myFilter = filter (not . (`elem` ["the", "a", "an"])) . myWords

-- Zipping exercises
-- 1
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

-- 2
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- 3
zip = zipWith (,)


-- Chapter exercises

-- Data.Char

-- 1
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- 2
keepUpper = filter isUpper

-- 3
capitalize x = (toUpper . head $ x) : tail x

-- 4
upperWord [] = []
upperWord (x:xs) = toUpper x : upperWord xs

-- 5
firstLetterCapitalized = toUpper . head

-- 6 - Already done with 5

-- Ciper - Check `ciper.hs`

-- Writing your own standard functions

-- 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = (e == x) || myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (e ==)

-- 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

-- 7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Empty list"
myMaximumBy cmp (x:xs) = go cmp xs x
  where go cmp (x:xs) max =
          if cmp x max > LT
          then go cmp xs x
          else go cmp xs max
        go cmp [] max = max

-- 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Empty list"
myMinimumBy cmp (x:xs) = go cmp xs x
  where go cmp (x:xs) min =
          if cmp x min == LT
          then go cmp xs x
          else go cmp xs min
        go cmp [] min = min

-- 10
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
