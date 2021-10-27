 -- I dont't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}

module Chapter_18 where

import GHC.Base (join)

-- The answer is the exercise

{-
Write bind in terms of fmap and join. Fear is the mind-killer, friend. You can
do it.
-}

-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind = join . fmap

-- Implement the Either Monad.

data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second $ f b
  fmap _ (First a) = First a

instance Applicative (Sum a) where
  pure x = Second x
  First a <*> _ = First a
  _ <*> First a = First a
  Second f <*> Second x = Second $ f x

instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second b >>= f = f b
