module TypeInference1 where

f :: Num a => a -> a -> a
f x y = x + y + 3

f' x y = x + y + 3
