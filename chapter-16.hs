module Chapter_16 where

-- Exercises: Be Kind

{-
Given a type signature, determine the kinds of each type variable:

1. What’s the kind of a?
a -> a

a :: *

2. What are the kinds of b and T ? (The T is capitalized on purpose!)
a -> b a -> T (b a)

T :: * -> *

3. What’s the kind of 𝑐?
c a b -> c b a

c :: * -> * -> *
-}

-- Wait, how does that even typecheck?

{-

(.) :: (b -> c) -> (a -> b) -> a -> c

fmap :: Functor f => (m -> n) -> f m -> f n

fmap :: Functor g => (x -> y) -> g x -> g y

Substituting both fmap signatures in dot (.) :

((m -> n) -> f m -> f n) -> ((x -> y) -> g x -> g y) -> a -> c
-----------------------     ------------------------
      b -> c                        a -> b

From this we can infer:

b ~ m -> n
c ~ f m -> f n

a ~ x -> y
b ~ g x -> g y

From this we can infer:

b ~ m -> n ~ g x -> g y

m ~ g x
n ~ g y

Subsituting values for a, c :

((m -> n) -> f m -> f n) -> ((x -> y) -> g x -> g y) -> (x -> y) -> (f m -> f n)
-----------------------     ------------------------    --------    ------------
      b -> c                        a -> b                 a              c

Substuting values for m, n :

((g x -> g y) -> (f g x) -> (f g y)) -> ((x -> y) -> (g x -> g y)) -> (x -> y) -> (f (g x) -> f (g y))
------------------------------------    --------------------------    --------    --------------------
            b -> c                              a -> b                    a                c

So when you use (fmap . fmap), the first two arguments are gives so `b -> c` and
`a -> b` is applied so the signature of the resulting function will be:

(x -> y) -> f (g x) -> f (g y)

Which matches the type you get when you write `:t (fmap . fmap)` in ghci.

That's how this type checks!

-}

-- Exercises: Heavy Lifting

{-
Add fmap, parentheses, and function composition to the expression as needed for
the expression to typecheck and produce the expected result. It may not always
need to go in the same place, so don’t get complacent.
-}

-- 1

a = fmap (+1) $ read "[1]" :: [Int]

-- 2

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3

c = fmap (* 2) (\x -> x - 2)

-- 4

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap ((read . ("123" ++)) . show) ioi
    in fmap (*3) changed
