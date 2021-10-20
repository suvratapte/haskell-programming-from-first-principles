 -- I dont't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}

module Chapter_16 where

import Test.QuickCheck (quickCheck, Arbitrary (arbitrary))
import Test.QuickCheck.Function

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
--
(.) :: (b -> c) -> (a -> b) -> a -> c --
--
fmap :: Functor f => (m -> n) -> f m -> f n --
--
fmap :: Functor g => (x -> y) -> g x -> g y --
--
Substituting both fmap signatures in dot (.) : --
--
((m -> n) -> f m -> f n) -> ((x -> y) -> g x -> g y) -> a -> c --
-----------------------     ------------------------ --
b -> c                        a -> b --
--
From this we can infer: --
--
b ~ m -> n --
c ~ f m -> f n --
--
a ~ x -> y --
b ~ g x -> g y --
--
From this we can infer: --
--
b ~ m -> n ~ g x -> g y --
--
m ~ g x --
n ~ g y --
--
Subsituting values for a, c : --
--
((m -> n) -> f m -> f n) -> ((x -> y) -> g x -> g y) -> (x -> y) -> (f m -> f n) --
-----------------------     ------------------------    --------    ------------ --
b -> c                        a -> b                 a              c --
--
Substuting values for m, n : --
--
((g x -> g y) -> (f g x) -> (f g y)) -> ((x -> y) -> (g x -> g y)) -> (x -> y) -> (f (g x) -> f (g y)) --
------------------------------------    --------------------------    --------    -------------------- --
b -> c                              a -> b                    a                c --
--
So when you use (fmap . fmap), the first two arguments are gives so `b -> c` and --
`a -> b` is applied so the signature of the resulting function will be: --
--
(x -> y) -> f (g x) -> f (g y) --
--
Which matches the type you get when you write `:t (fmap . fmap)` in ghci. --
--
That's how this type checks! --
--
-}

-- Exercises: Heavy Lifting

  {-
Add fmap, parentheses, and function composition to the expression as needed for --
the expression to typecheck and produce the expected result. It may not always --
need to go in the same place, so don’t get complacent. --
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

-- QuickChecking Functor instances

-- Functor laws
-- fmap id = id
-- fmap (p . q) = (fmap p) . (fmap q)

prop_functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
prop_functorIdentity f = fmap id f == id f

prop_functorComposition :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
prop_functorComposition f g a = fmap (f . g) a == (fmap f . fmap g) a

test1 :: [Int] -> Bool
test1 x = prop_functorIdentity x

test2 :: [String] -> Bool
test2 = prop_functorComposition (++ "hi") (++ "hello")

prop_functorComposition' :: (Eq (f c), Functor f) => Fun b c -> Fun a b -> f a -> Bool
prop_functorComposition' (Fun _ f) (Fun _ g) a = fmap (f . g) a == (fmap f . fmap g) a

-- Exercsies: Instances of Func

{-
Implement Functor instances for the following datatypes. Use the QuickCheck
properties we showed you to validate them.
-}

-- 1
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

-- 2
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

-- 3
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    Two a <$> arbitrary

-- 4
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

-- 5
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three' a b <$> arbitrary

-- 6
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four a b c <$> arbitrary

-- 7
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    Four' a1 a2 a3 <$> arbitrary

-- 8
-- Can you implement one for this type? Why? Why not?
-- data Trivial = Trivial

-- Can't implement because this type is not a * -> * type.
-- Therefore, there is no structure in this type.
-- So it cannot be a functor

main = do
  quickCheck (prop_functorIdentity :: Identity Int -> Bool)
  quickCheck (prop_functorIdentity :: Pair Char -> Bool)
  quickCheck (prop_functorIdentity :: Two Int Char -> Bool)
  quickCheck (prop_functorIdentity :: Three Int Char Double -> Bool)
  quickCheck (prop_functorIdentity :: Three' String Int -> Bool)
  quickCheck (prop_functorIdentity :: Four String Int Double Bool -> Bool)
  quickCheck (prop_functorIdentity :: Four' Int Char -> Bool)
