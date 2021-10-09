-- Chapter 14

-- Disabling warnings since the exercises are not really practical

module Chapter_14 where

import Data.List (intercalate, sort)
import Test.Hspec
import Test.QuickCheck

-- Validating numbers into words

-- Intermission: Short Exercise

  {-
In the Chapter Exercises at the end of Recursion, you were given this exercise: --
--
Write a function that multiplies two numbers using recursive sum- mation. The --
type should be (Eq a, Num a) => a -> a -> a although, depending on how you do --
it, you might also consider adding an Ord constraint. --
--
If you still have your answer, great! If not, rewrite it and then write hspec --
tests for it. --
-}

multBy :: (Num a, Eq a) => a -> a -> a
multBy x 0 = 0
multBy x y = x + multBy x (y - 1)

main1 :: IO ()
main1 = hspec $ do
  describe "multBy" $ do
    it "5 multiplied by 0 is 0" $ do
      multBy 5 0 `shouldBe` 0
    it "10 multiplied by 1 is 10" $ do
      multBy 10 1 `shouldBe` 10
    it "20 multiplied by 10 is 200" $ do
      multBy 20 10 `shouldBe` 200

-- Chapter exercises

  {-
Remember the “numbers into words” exercise in Recursion? You’ll be writing tests --
to validate the functions you wrote. --
-}

-- Pasting the functions here:

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

main2 :: IO ()
main2 = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "return [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

-- Using QuickCheck

-- 1

-- for a function
half x = x / 2

-- this property should hold
halfIdentity = (*2) . half

-- 2

-- for any list you apply sort to  this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_sorting xs = listOrdered xs == (sort xs == xs)

-- 3

-- Now we’ll test the associative and commutative properties of addition:

genericAssociative :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
genericAssociative f x y z = x `f` (y `f` z) == (x `f` y) `f` z

genericCommutative :: (Eq a) => (a -> a -> a) -> a -> a -> Bool
genericCommutative f x y = x `f` y == y `f` x

prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative = genericAssociative (+)

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative = genericCommutative (+)

-- 4
-- Now do the same for multiplication.

prop_multAssociative :: Int -> Int -> Int -> Bool
prop_multAssociative = genericAssociative (*)

prop_multCommutative :: Int -> Int -> Bool
prop_multCommutative = genericCommutative (*)

-- 5

-- We mentioned in one of the first chapters that there are some laws involving
-- the relationship of quot and rem and div and mod. Write QuickCheck tests to
-- prove them

prop_quotRem :: Int -> NonZero Int -> Bool
prop_quotRem x (NonZero y) = quot x y * y + rem x y == x

prop_divMod :: Int -> NonZero Int -> Bool
prop_divMod x (NonZero y) = div x y * y + mod x y == x

-- 6

-- Is (^) associative? Is it commutative? Use QuickCheck to see if the computer
-- can contradict such an assertion.

-- Yes, both of these will fail:

prop_powerAssociative :: Int -> Int -> Int -> Bool
prop_powerAssociative = genericAssociative (^)

prop_powerCommutative :: Int -> Int -> Bool
prop_powerCommutative = genericCommutative (^)

-- 7

-- Test that reversing a list twice is the same as the identity of the list:

prop_doubleReverse :: [Int] -> Bool
prop_doubleReverse xs = (reverse . reverse) xs == xs

-- 8

-- Write a property for the definition of ($).

prop_dollar :: (Eq b) => (a -> b) -> a -> Bool
prop_dollar f a = f a == (f $ a)

prop_dollarDouble :: Int -> Bool
prop_dollarDouble = prop_dollar (* 2)

prop_dot :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_dot f g a = (f . g) a == f (g a)

prop_stringToIntToTwice :: String -> Bool
prop_stringToIntToTwice = prop_dot (* 2) length

-- 9
-- See if these two functions are equal:

prop_foldrCons :: [Int] -> [Int] -> Bool
prop_foldrCons xs ys = foldr (:) xs ys == xs ++ ys

-- Fails. Could pass if we flip (++).

prop_foldrConcat :: [[Int]] -> Bool
prop_foldrConcat xs = foldr (++) [] xs == concat xs

-- Passes.

-- 10
-- Hm. Is that so?

prop_lengthTake :: [Int] -> NonNegative Int -> Bool
prop_lengthTake xs (NonNegative n) = length (take n xs) == n

-- Fails.

-- Finally, this is a fun one. You may remember we had you com- pose read and
-- show one time to complete a “round trip.” Well, now you can test that it
-- works:

prop_readShow :: Int -> Bool
prop_readShow x = read (show x) == x
