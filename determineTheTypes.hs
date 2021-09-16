{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- simple example
example = 1

-- 1

-- a
(* 9) 6 -- Num a => a

-- b
head [(0, "doge"), (1, "kitteh")] -- Num a => (a, [Char])

-- c
head [(0 :: Integer, "doge"), (1, "kitteh")] -- (Integer, [Char])

-- d
if False then True else False -- Bool

-- e
length [1, 2, 3, 4, 5] -- Int

-- f
(length [1, 2, 3, 4]) > (length "TACOCAT") -- Bool

-- 2
x = 5
y = x + 5
w = y * 10
-- Int

-- 3
z y = y * 10
-- Num a => a -> a

f = 4 / y
-- Fractional a => a -> a

-- 5
x = "Julie"
y = " <3 "
z = "Haskell"
f = x ++ y ++ z
-- [Char]

-- Does it compile?

-- 1
bigNum = (^) 5 $ 10
wahoo = bigNum $ 10
-- Won't work because `bigNum` is not a value; Not a function that takes one
-- argument.
-- To fix it:
bigNum x = (^) 5 $ x
wahoo = bigNum $ x

-- 2
x = print
y = print "woohoo!"
z = x "hello world"
-- Will work!

-- 3
a = (+)
b = 5
c = b 10
d = c 200
-- Won't work because `b 10` is not valid function application. `b` is not a
-- function.
-- To fix it:
a = (+)
b = 5
c = a 10
d = c 200

-- 4
a = 12 + b
b = 10000 * c
-- Won't work because `c` is not in scope.
-- To fix it:
a = 12 + b
c = 1
b = 10000 * c

-- Type variable or specific type constructor

-- 1 (Solved example)
-- 2
f :: zed -> Zed -> Blah -- fully polymorphic -> concrete -> concrete

-- 3
f :: Enum b => a -> b -> C -- fully polymorphic -> constrained polymorphic -> concrete

-- 4
f :: f -> g -> C -- fully polymorphic -> fully polymorphic -> concrete

-- Write a type signature

-- 1
functionH :: [a] -> a
functionH (x:_) = x

-- 2
functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- 3
functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function

-- 1
i :: a -> a
i = id

-- 2
c :: a -> b -> a
c x _ = x

-- 3
c'' :: b -> a -> b
c'' = c

-- 4
c' :: a -> b -> b
c' _ y = y

-- 5
r :: [a] -> [a]
r = id
r x = x ++ x
-- r x = any operations with lists that return the list of the same type

-- 6
co :: (b -> c) -> (a -> b) -> a -> c
co = (.)

-- 7
a :: (a -> c) -> a -> a
a _ a = a

-- 8
a' :: (a -> b) -> a -> b
a' = ($)

-- Fix it

-- 1
module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"

-- 2
sing = if (x < y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"

-- 3
module Arith3Broken where

main :: IO ()
main = do
  print $ 1 + 2
  putStrLn (show 10)
  print (negate 1)
  print ((+) 0 blah)
  where blah = negate 1

-- Type-Kwon-Do

-- 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

-- 2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- 3
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (a, b) = (xz a, yz b)

-- 4
munge ::
  (x -> y)
  -> (y -> (w, z))
  -> x
  -> w

munge xy ywz = fst . ywz . xy
