-- Disabling warnings since the exercises are not really practical
{-# OPTIONS_GHC -Wno-all #-}

module Chapter_14 where

import Data.List (intercalate)
import Test.Hspec
import Test.QuickCheck

-- Validating numbers into words

-- Intermission: Short Exercise

{-
In the Chapter Exercises at the end of Recursion, you were given this exercise:

Write a function that multiplies two numbers using recursive sum- mation. The
type should be (Eq a, Num a) => a -> a -> a although, depending on how you do
it, you might also consider adding an Ord constraint.

If you still have your answer, great! If not, rewrite it and then write hspec
tests for it.
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
Remember the “numbers into words” exercise in Recursion? You’ll be writing tests
to validate the functions you wrote.
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

intGen :: Gen Int
intGen = return arbitrary

prop_doubleHalf :: Property
prop_doubleHalf = forAll (do return (arbitrary :: Int)) (\number -> halfIdentity number == number)
