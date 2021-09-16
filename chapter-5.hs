-- Chapter 5 exercises

-- Exercises - Type Matching

-- a - c
-- b - d
-- c - b
-- d - a
-- e - e

-- Exercises - Type Arguments

-- 1 - a
-- 2 - d
-- 3 - d
-- 4 - c
-- 5 - a
-- 6 - e
-- 7 - d
-- 8 - a
-- 9 - c

-- Exercises - Parametricity

-- 1 - Can't do anything here!

-- 2
f :: a -> a -> a
f x y = x
f x y = y

-- 3 - It has only one implementation and the behavior does not change when the
-- type of `a` or `b` change.
f' :: a -> b -> b
f' x y = y

-- Exercises - Apply Yourself

-- 1 - [Char] -> [Char]
-- 2 - Fractional a => a -> a
-- 3 - Int -> [Char]
-- 4 - Int -> Bool
-- 5 - Char -> Bool

-- Chapter exercises

-- 1 - a, b, c
-- 2 - a
-- 3 - b
-- 4 - c

-- Look at the file `determineTheType.hs`.
