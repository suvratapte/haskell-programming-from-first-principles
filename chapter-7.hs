-- Chapter 7 exercises

-- Grab bag

-- 1 - a, b, c, d
-- 2 - d
-- 3 -
-- a
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- b
addFive = \x y -> (if x > y then y else x) + 5

-- c
mflip f x y = f y x

-- Variety Pack

-- 1
-- a - (a, b) -> a
-- b - [Char]. No, it is not the same as k1 or k3.
-- c - k1 and k3

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

-- Case Practice

-- 1
functionC x y = case x > y of
  True -> x
  False -> y

-- 2
ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

-- 3
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  _ -> 0 -- _ or EQ both will work as EQ is the only remaining case.

-- Artful Dodgy

-- 1 - 1
-- 2 - 11
-- 3 - 22
-- 4 - 21
-- 5 - 12
-- 6 - 11
-- 7 - 21
-- 8 - 21
-- 9 - 22
-- 10 - 31
-- 11 - 23

-- Guard Duty
-- 1 - `otherwise` will match first as that's the broadest match.
-- 2 - It won't return A because the C case will match first.
-- 3 - b
-- 4 - Any list
-- 5 - pal :: Eq a => [a] -> Bool
-- 6 - c
-- 7 - Anything that has Ord and Num.
-- 8 - numbers :: (Ord a, Num a, Num p) => a -> p

-- Chapter exercises - Multiple choice

-- 1 - d
-- 2 - b
-- 3 - d
-- 4 - b
-- 5 - a

-- Let's write code

-- 1
-- a
tensDigit :: Integral a => a -> a
tensDigit x = d
  where (xLast, _) = x `divMod` 10
        (_, d) = xLast `divMod` 10

-- b - Yes

-- c
hunsDigit :: Integral a => a -> a
hunsDigit = (`mod` 10) . (`div` 100)

-- 2
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

foldBool3Case :: a -> a -> Bool -> a
foldBool3Case x y c =
  case c of
    True -> y
    False -> x

foldBool3Guard :: a -> a -> Bool -> a
foldBool3Guard x y c
  | c == False = x
  | c == True = y

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g fab (a, c) = (fab a, c)


-- 4
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 5
roundTripPointFree :: (Show a, Read a) => a -> a
roundTripPointFree = read . show

-- 6
roundTripPointFree' :: (Show a, Read b) => a -> b
roundTripPointFree' = read . show
