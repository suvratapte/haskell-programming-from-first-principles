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
