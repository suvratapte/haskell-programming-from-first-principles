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
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

-- Exercise: Reader Monad

-- 1
-- Implement the Reader Monad.

instance Monad (Reader r) where
  Reader ra >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

-- 2
-- Rewrite the monadic getDogRM to use your Reader datatype.

getDogRM :: Reader Person Dog
getDogRM = do
  n <- asks dogName
  a <- asks address
  return $ Dog n a

-- Chapter Exercises

{-
A warm-up stretch

These exercises are designed to be a warm-up and get you using some of the stuff
we’ve learned in the last few chapters. While these exercises comprise code
fragments from real code, they are simpli- fied in order to be discrete
exercises. That will allow us to highlight and practice some of the type
manipulation from Traversable and Reader, both of which are tricky.  The first
simplified part is that we’re going to set up some toy data; in the real
programs these are taken from, the data is coming from somewhere else — a
database, for example. We just need some lists of numbers. We’re going to use
some functions from Control.Applicative and Data.Maybe, so we’ll import those at
the top of our practice file. We’ll call our lists of toy data by common
variable names for simplicity.

module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

The next thing we want to do is write some functions that zip those lists
together and use lookup to find the value associated with a specified key in our
zipped lists. For demonstration purposes, it’s nice to have the outputs be
predictable, so we recommend writing some that are concrete values, as well as
one that can be applied to a variable:

lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = undefined

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = undefined

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = undefined

Now we want to add the ability to make a Maybe (,) of values using Applicative.
Have x1 make a tuple of xs and ys, and x2 make a tuple of of ys and zs. Also,
write x3 which takes one input and makes a tuple of the results of two
applications of z' from above.

x1 :: Maybe (Integer, Integer)
x1 = undefined

x2 :: Maybe (Integer, Integer)
x2 = undefined

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = undefined

Next, we’re going to make some helper functions. Let’s use uncurry to allow us
to add the two values that are inside a tuple:

uncurry :: (a -> b -> c) -> (a, b) -> c

-- that first argument is a function
-- in this case, we want it to be addition -- summed is uncurry with addition as
-- the first argument

summed :: Num c => (c, c) -> c
summed = undefined

And now we’ll make a function similar to some we’ve seen before that lifts a
boolean function over two partially applied functions:

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = undefined

Finally, we’ll be using fromMaybe in the main exercise, so let’s look at that:

fromMaybe :: a -> Maybe a -> a

You give it a default value and a Maybe value. If the Maybe value is a Just a,
it will return the 𝑎 value. If the value is a Nothing, it returns the default
value instead:

*ReaderPractice> fromMaybe 0 xs
6
*ReaderPractice> fromMaybe 0 zs
0

Now we’ll cobble together a main, so that in one call we can execute several
things at once.

main :: IO ()
main = do
  print $
    sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z

When you run this in GHCi, your results should look like this:

*ReaderPractice> main
Just [3,2,1]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
Just [6,9]
Just 15
Nothing
True
[True,False,False]

Next, we’re going to add one that combines sequenceA and Reader in a somewhat
surprising way (add this to main):

print $ sequenceA [(>3), (<8), even] 7

The type of sequenceA is
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
-- so in this:
sequenceA [(>3), (<8), even] 7
-- f ~ (->) a and t ~ []

We have a Reader for the Applicative (functions) and a traversable for the
list. Pretty handy. We’re going to call that function sequA for the purposes of
the following exercises:

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

And henceforth let

summed <$> ((,) <$> xs <*> ys)

OK, your turn. Within the main above, write the following (you can delete
everything after do now if you prefer — just remember to use print to be able to
print the results of what you’re adding):

1. fold the boolean conjunction operator over the list of results of sequA (applied to some value).
2. apply sequA to s'; you’ll need fromMaybe.
3. apply bolt to ys; you’ll need fromMaybe.

-}

-- Solution: Look at the `ReaderPractice.hs` file in this directory.
