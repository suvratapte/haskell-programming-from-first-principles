-- Chapter 4 exercises

-- Exercises - Mood Swing

-- 1 - Mood.
-- 2 - reverse or `Woot`.
-- 3 - Type signature can't have values. It should have types.
-- 4 -

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

-- 5 - Done.

-- Exercises - Find the Mistakes

-- 1 -
not True && True

-- 2 -
not (x == 6)

-- 3 -
(1 * 2) > 5

-- 4 -
"Merry" > "Happy"

-- 6 -
['1', '2', '3'] ++ "look at me!"

-- Chapter exercises

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- 1 - `length` must be [a] -> Int

-- 2
-- a - 5
-- b - 3
-- c - 2
-- d - 5

-- 3 -
-- `6 / length [1, 2, 3]` must be resulting in an error since `legnth` returns
-- `Int` and we want a `Fractional` cosntraint.

-- 4 - We can use the `div` function.

-- 5 - Type will be `Bool` and the result will be `True`.

-- 6 - Type will be `Bool` and the result will be `False`.

-- 7
-- a - Will work - True
-- b - Wont' work - List must have elements with the same type.
-- c - Will work - 5
-- d - Will work - False
-- e - Won't work - && needs two Bools.

-- 8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

-- 9
myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else (-x)

-- 10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Correcting syntax

-- 1
x = (+)
f' xs = x w 1
  where w = length xs

-- 2
\x -> x

-- 3
f'' (a, b) = a

-- Match the function names to their types
-- 1 - c
-- 2 - b
-- 3 - a
-- 4 - d
