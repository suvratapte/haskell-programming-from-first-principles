-- Chapter 6 exercises

-- Exercises: Eq Instances

--1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn int) (TisAn int') = int == int'

-- 2
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b')
    = a == a' && b == b'

-- 3
data StringOrInt =
    TisAnInt Integer
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt a') = a == a'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False

-- 4
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'

-- 5
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

-- 6
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

-- 7
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye a) (Goodbye a') = a == a'
  (==) _ _ = False

-- Exercises - Tuple Experiment

-- `quotRem` has the return type `(a, a)` so it should be returning quotient and
-- remainder.

-- `divMod` has the return type `(a, a)` so it should be returning division and
-- modulo.

-- Exercises - Will They Work?

-- 1 - Will work - 5
-- 2 - Will work - LT
-- 3 - Won't work - The first argument to `compare` will fix the type and the
--                  second argument won't match the type.
-- 4 - Will work - False

-- Chapter exercises

-- Multiple choice

-- 1 - c
-- 2 - a, b
-- 3 - a
-- 4 - c
-- 5 - a

-- Does it typecheck?

-- 1
data Person = Person Bool

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- Won't typecheck because `Person` does not have an instance of Show.

-- Fix: Add "deriving Show" at the end of the datatype.

-- 2
data Mood = Blah | Woot
  deriving Show

settleDown x = if x == Woot then Blah else x

-- Won't typecheck because `Mood` does not have an instance of Eq.

-- Fix: Add "deriving Eq" at the end of the datatype.

-- 3
-- a - Mood
-- b - Expected type: Mood, Actual type: Int
-- c - Will return in an error since Ord is not derived or implmemented.

-- 4 - Will work.

-- Given a datatype declaration, what can we do?

-- 1 - Won't work - It needs Rocks and Yeah; not String and Bool.
-- 2 - Will work.
-- 3 - Will work.
-- 4 - Wont' work - Papu doesn't have Ord.

-- Match the types

-- 1 - I don't understand why this is not working. `a` is a polymorphic type so
--     it should work.
-- 2 - I don't understand why this doesn't work: f :: Num a => a; f = (1.0 :: Double)
-- 3 - Works.
-- 4 - Works.
-- 5 - Works.
-- 6 - Works.
-- 7 - Works.
-- 8 - Won't work - Because we are returning an Int.
-- 9 - Works.
-- 10 - Works.
-- 11 - Won't work - mySort only works with Char.

-- Type-Kwon-Do Two: Electric Typealoo

-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fab a b = (fab a) == b
-- Another clever way with currying:
-- chk fab a = (==) (fab a)

-- Instead of `==`, we can use `/=` as well.


-- 2
arith ::
     Num b
  => (a -> b)
  -> Integer
  -> a
  -> b

arith fab _ = fab
