 -- I don't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter_16 where

import Test.QuickCheck (quickCheck, Arbitrary (arbitrary))
import Test.QuickCheck.Function
import GHC.Arr

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

-- Exercise: Possibly

{-
Write a Functor instance for a datatype identical to Maybe. We’ll use our own
datatype because Maybe already has a Functor instance and we cannot make a
duplicate one.
-}

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

--  Short Exercise

-- 1
{-
Write a Functor instance for a datatype identical to Either. We’ll use our own
datatype because Either has a Functor instance.
-}

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

-- 2
{-
Why is a Functor instance that applies the function only to First, Either’s --
Left, impossible? We covered this earlier. --
-}

{-
It is impossible because the kind of Sum or Either is * -> * => *.
Applying the first type makes it * -> *. So the first type is the structure
on which the functor function will apply.

However, we can implement it with a newtype like this:
-}

newtype InvertedEither b a = InvertedEither (Either a b)
  deriving (Eq, Show)

instance Functor (InvertedEither b) where
  fmap _ (InvertedEither (Right b)) = InvertedEither $ Right b
  fmap f (InvertedEither (Left a)) = InvertedEither $ Left $ f a

test3 = fmap (+1) $ InvertedEither $ Left 10
test4 = fmap (+1) $ InvertedEither $ Right "hello"

-- Something to think about:

{-
Can we generalize this to any * -> * -> * type?
No, because you need to have a concrete type at some point.
-}

-- 16.17 Chapter exercises

-- Determine if a valid Functor can be written for the datatype provided.

-- 1
-- Commented because it would override the default bool.
-- data Bool = False | True

-- No. The kind is *.

-- 2
data BoolAndSomethingElse a = False' a | True' a

-- Yes. The kind is * -> *.

-- 3
data BoolAndMaybeSomethingElse a = Falsish | Truish a

-- Yes. The kind is * -> *.

-- 4
-- Use the kinds to guide you on this one, don’t get too hung up on the details.

newtype Mu f = InF { outF :: f (Mu f) }

-- Yes. The kind is * -> *.
-- But the type itself doesn't make much sense.
-- TODO: Check why this compiles?

-- 5

-- Again, follow the kinds and ignore the unfamiliar parts

data D = D (Array Word Word) Int Int

-- No. The kind is * -> *.

-- Rearrange the arguments to the type constructor of the datatype so the
-- Functor instance works.

-- 1
data Sum' b a = First' a | Second' b

instance Functor (Sum' e) where
  fmap f (First' a) = First' (f a)
  fmap _ (Second' b) = Second' b

-- 2
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3
data More b a = L a b a | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

{-
Keeping in mind that it should result in a Functor that does the following:

Prelude> fmap (+1) (L 1 2 3)
L 2 2 4

Prelude> fmap (+1) (R 1 2 3)
R 1 3 3
-}

-- Write Functor instances for the following datatypes.

-- 1
data Quant a b = Finance | Desk a | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor $ f b
  fmap _ (Desk a) = Desk a
  fmap _ Finance = Finance

-- 2
-- No, it’s not interesting by itself.

data K a b = K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3

newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

newtype K' a b = K' a

-- should remind you of an instance you've written before:

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a))

-- 4
data EvilGoateeConst a b = GoatyConst b
  deriving (Eq, Show)

{-
You thought you'd escaped the goats by now didn't you? Nope. No, it doesn’t do --
anything interesting. No magic here or in the previous exercise. If it works, --
you succeeded. --
-}

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

-- 5
-- Do you need something extra to make the instance work?

-- No, you can make it work as long as f is a functor

data LiftItOut f a = LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- 6

data Parappa f g a = DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7
-- Don’t ask for more typeclass instances than you need. You canlet GHC tell you
-- what to do.

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa $ fmap f gb

-- 8

data Notorious g o a t = Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 8
-- You’ll need to use recursion.

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

-- 10
-- A tree of goats forms a Goat-Lord, fearsome poly-creature.

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)
-- A VERITABLE HYDRA OF GOATS

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats a1 a2 a3) = MoreGoats (fmap f a1) (fmap f a2) (fmap f a3)

-- 11

{-
You’ll use an extra functor for this one, although your solution might do it
monomorphically without using fmap. Keep in mind that you will probably not be
able to validate this one in the usual manner. Do your best to make it work.
-}

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read sa) = Read (fmap f sa)
