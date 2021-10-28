 -- I don't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}

module Chapter_18 where

import GHC.Base (join, liftA3, Applicative (liftA2))

-- The answer is the exercise

{-
Write bind in terms of fmap and join. Fear is the mind-killer, friend. You can
do it.
-}

-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

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

-- Chapter Exercises

{-
Write Monad instances for the following types. Use the QuickCheck properties we
showed you to validate your instances.
-}

-- 1
-- Welcome to the Nope Monad, where nothing happens and nobody cares.

data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

-- 2
data PhhhbbtttEither b a = Left' a | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' $ f a
  fmap _ (Right' b) = Right' b

instance Monoid b => Applicative (PhhhbbtttEither b) where
  pure a = Left' a
  Right' b <*> Right' b' = Right' $ b `mappend` b'
  Right' b <*> _ = Right' b
  _ <*> Right' b = Right' b
  Left' f <*> Left' a = Left' $ f a

instance Monoid b => Monad (PhhhbbtttEither b) where
  return = pure
  Left' a >>= f = f a
  Right' b >>= _ = Right' b

-- 3
-- Write a Monad instance for Identity.

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

-- 4
{-
This one should be easier than the Applicative instance was. Remember to use the
Functor that Monad requires, then see where the chips fall.
-}

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> Nil = Nil
  Nil <> x = x
  x <> Nil = x
  Cons x xs <> y = Cons x $ xs <> y

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = fmap f xs <> (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)

{-
Write the following functions using the methods provided by Monad and
Functor. Using stuff like identity and composition is fine, but it has to
typecheck with types provided.
-}

-- 1
j :: Monad m => m (m a) -> m a
j = (>>= id)

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 abc ma mb = ma >>= (\a -> mb >>= (\b -> return $ abc a b))

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a ma mab = mab >>= \ab -> ma >>= \a -> return $ ab a

-- 5
-- You’ll need recursion for this one.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = f a >>= \b -> (b :) <$> meh as f

-- 6
flipType :: (Monad m) => [m a] -> m [a]
flipType = (`meh` id)
