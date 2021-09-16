-- Chapter 3 exercises

-- Exercises: Scope

-- 1 - Yes
-- 2 - No
-- 3 - No
-- 4 - Yes

-- Exercises: Syntax Errors

-- 1 - No. Correct syntax : (++) [1, 2, 3] [4, 5, 6]
-- 2 - No. Correct syntax : "<3" ++ " Haskell"
-- 3 - Yes.

-- Chatper Exercises

-- Reading syntax

-- 1
-- a - Yes
-- b - No - (++) [1, 2, 3] [4, 5, 6]
-- c - Yes
-- d - No - "hello" ++ "world"
-- e - No - "hello" !! 4
-- f - Yes
-- g - No - take 4 "lovely"
-- h - Yes

-- 2
-- a - d
-- b - c
-- c - e
-- d - a
-- e - b

-- Building functions

-- 1

-- a
-- "Curry is awesome" ++ "!"

-- b
-- "Curry is awesome!" !! 4

-- c
-- drop 9 "Curry is awesome!"

-- 2

-- a
-- exclaim x = x ++ "!"

-- b
-- getFourthIndex x = x !! 4

-- c
-- dropNine x = drop 9 x

-- 3
-- thirdLetter :: String -> Char
-- thirdLetter x = x !! 2

--4
-- letterIndex :: Int -> Char
-- letterIndex i = "Curry is awesome!" !! i

-- 5
-- rvrs x =
--   let
--     firstWord = take 5 x
--     secondWord = take 2 $ drop 6 x
--     thirdWord = drop 9 x
--     in
--     thirdWord ++ " " ++ secondWord ++ " " ++ firstWord

-- All the above exercises are commented so that the following one works.

-- 6

module Reverse where

rvrs :: String -> String
rvrs x =
  let
    firstWord = take 5 x
    secondWord = take 2 $ drop 6 x
    thirdWord = drop 9 x
    in
    thirdWord ++ " " ++ secondWord ++ " " ++ firstWord

main :: IO ()
main = print $ rvrs "Curry is awesome"
