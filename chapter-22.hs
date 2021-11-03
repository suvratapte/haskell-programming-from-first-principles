 -- I don't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}

module Chapter_22 where

import Control.Applicative
import Data.Char

boop = (* 2)
doop = (+ 10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

-- For my understanding:

{-

instance Functor ((->) e) where
  fmap fab ea = fab . ea

instance Applicative ((->) e) where
  (<*>) :: f (a -> b) -> f a -> f b
          (e -> a -> b) -> (e -> a) -> (e -> b)
  eab <*> ea = \e -> eab e (ea e)

(+)  :: Integer -> Integer -> Integer
boop :: Integer -> Integer
doop :: Integer -> Integer

fmap (+) doop = (+) . doop

(.) :: (b -> c) -> (a -> b) -> a -> c

(+) :: Integer -> Integer -> Integer

(Integer -> Integer -> Integer) ~ (b -> c)

b ~ Integer
c ~ Integer -> Integer

Integer -> Integer ~ (a -> b)
a ~ Integer
b ~ Integer

(+) . doop :: a -> c :: Integer -> Integer -> Integer

fmap (+) doop :: Integer -> Integer -> Integer
(+) <$> doop :: Integer -> Integer -> Integer

(+) <$> doop <*> boop :: Integer -> Integer

-}

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

-- Short Exercise: Warming Up

{-
We’ll be doing something here very similar to what you saw above, to give you
practice and try to develop a feel or intuition for what is to come. These are
similar enough to what you just saw that you can almost copy and paste, so try
not to overthink them too much.
-}

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap

{-
Now we want to return the results of cap and rev both, as a tuple, like this:

Prelude> tupled "Julie"
("JULIE","eiluJ")

-- or

Prelude> tupled' "Julie"
("eiluJ","JULIE")
-}

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
  c <- cap
  r <- rev
  return (c, r)

tupledDo' :: [Char] -> ([Char], [Char])
tupledDo' = do
  c <- cap
  r <- rev
  return (r, c)

tupledM :: [Char] -> ([Char], [Char])
tupledM = cap >>= \c -> rev >>= \r -> return (r, c)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = rev >>= \r -> cap >>= \c -> return (c, r)

-- Exercise: Ask

{-
Implement the following function. If you get stuck, remember it’s less
complicated than it looks. Write down what you know. What do you know about the
type a? What does the type simplify to? How many inhabitants does that type
have? You’ve seen the type before.
-}

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

--

newtype HumanName = HumanName String
  deriving (Eq, Show)

newtype DogName = DogName String
  deriving (Eq, Show)

newtype Address = Address String
  deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  }
  deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog p  = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- For my understanding:

{-
Dog :: DogName -> Address -> Dog
dogName :: Person -> DogName

fmap :: (a -> b) -> f a -> f b

-- Our f is ((->) r)

fmap :: (a -> b) -> (r -> a) -> (r -> b)

-- So with `fmap Dog dogName`, we can say

-- (a -> b) ~ (DogName -> Address -> Dog)
-- a ~ DogName
-- b ~ Address -> Dog

-- (r -> a) ~ (Person -> DogName)
-- r ~ Person
-- a ~ DogName

-- So the type of the whole expression:
Dog <$> dogName :: Person -> Address -> Dog


(<*>) :: (e -> a -> b) -> (e -> a) -> (e -> b)

(Dog <$> dogName) <*> address

-- So

(e -> a -> b) ~ (Person  -> Address -> Dog)

(e -> a) ~ (Person -> Address)

(e -> b) ~ (Person -> Dog)

Dog <$> dogName <*> address :: Person -> Dog
-}

-- Exercise: Reading Comprehension

-- 1

-- Write liftA2 yourself. Think about it in terms of abstracting out the
-- difference between getDogR and getDogR' if that helps.

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 fabc fa fb = fabc <$> fa <*> fb

-- Point free (but more complex) :
-- myLiftA2 = ((<*>) .) . (<$>)

-- 2
--  Write the following function. Again, it is simpler than it looks.
asks :: (r -> a) -> Reader r a
asks f = Reader f

-- 3
{-
Implement the Applicative for Reader.

To write the Applicative instance for Reader, we’ll use an extension called
InstanceSigs. It’s an extension we need in order to assert a type for the
typeclass methods. You ordinarily cannot assert type signatures in
instances. The compiler already knows the type of the functions, so it’s not
usually necessary to assert the types in instances anyway. We did this for the
sake of clarity, to make the Reader type explicit in our signatures.
-}

-- I have not used InstanceSigs since I am comfortable with the default
-- signatures:

instance Functor (Reader r) where
  fmap fab (Reader ra) = Reader (fab . ra)

instance Applicative (Reader r) where
  pure x = Reader $ const x
  (Reader rab) <*> (Reader ra) = Reader (\r -> rab r (ra r))
