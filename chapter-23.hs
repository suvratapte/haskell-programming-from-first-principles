 -- I don't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE TupleSections #-}

module Chapter_23 where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad.Trans.State
import Control.Monad (replicateM, join)
import System.Random

-- newtype State s a =
--   State { runState :: s -> (a, s) }

-- Six-sided die
data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie x =
  case x of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    _ -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes =
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, s3) = randomR (1, 6) s2
  in
  (intToDie d1, intToDie d2, intToDie d3)

-- rollDie :: State StdGen Die
-- rollDie = state $ do
--   (n, s) <- randomR (1, 6)
--   return (intToDie n, s)

rollDie :: State StdGen Die
rollDie = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- Exercises: Roll Your Own

-- 1
-- Refactor rollsToGetTwenty into having the limit be a function argument.

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n sg = go 0 0 sg
  where
    go sum count sg
      | sum > n = count
      | otherwise =
          let (d, newSg) = randomR (1, 6) sg
          in
          go (sum + d) (count + 1) newSg

-- 2

-- Change rollsToGetN to recording the series of die that occurred in addition
-- to the count.

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n sg = go 0 (0, []) sg
  where
    go sum cds@(count, ds) sg
      | sum > n = cds
      | otherwise =
          let (d, newSg) = randomR (1, 6) sg
          in
          go (sum + d) (count + 1, intToDie d : ds) newSg

--

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi sas) = Moi $ \s -> let (a, s') = sas s in (f a, s')

instance Applicative (Moi s) where
  pure x = Moi (x,)
  Moi sfs <*> (Moi sas) =
    Moi $ \s ->
      let (f, s1) = sfs s
          (a, s2) = sas s1 -- ?? Is passing s1 here correct?
      in
      (f a, s2)

instance Monad (Moi s) where
  return = pure
  Moi sas >>= famb = join $ Moi $ \s -> let (a, s') = sas s in (famb a, s')

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n`mod`5 == 0 = "Buzz"
  | n`mod`3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]

-- Fizzbuzz Differently

  {-
It’s an exercise! Rather than changing the underlying data structure, fix our --
reversing fizzbuzz by changing the code in the following way: --
--
fizzbuzzFromTo :: Integer -> Integer -> [String] --
fizzbuzzFromTo = undefined --
--
Continue to use consing in the construction of the result list, but have it come --
out in the right order to begin with by enumerating the sequence backwards. This --
sort of tactic is more commonly how you’ll want to fix your code when you’re --
quashing unnecessary reversals. --
-}

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = map fizzBuzz $ enumFromThenTo to (to - 1) from

-- Chapter exercises

-- Write the following functions. You’ll want to use your own State type for
-- which you’ve defined the Functor, Applicative, and Monad.

-- 1
-- Construct a State where the state is also the value you return.

get' :: State s s
get' = state $ \s -> (s, s)

-- Expected output
-- Prelude> runState get' "curryIsAmaze"
-- ("curryIsAmaze","curryIsAmaze")

-- 2

-- Construct a State where the resulting state is the argument pro- vided and
-- the value is defaulted to unit.

put':: s -> State s ()
put' s = state $ const ((), s)

-- Prelude> runState (put' "blah") "woot"
-- ((),"blah")

-- 3
-- Run the State with 𝑠 and get the state that results.
exec' :: State s a -> s -> s
exec' sa = snd . runState sa

-- Prelude> exec' (put' "wilma") "daphne"
-- "wilma"
-- Prelude> exec' get' "scooby papu"
-- "scooby papu"

-- 4
-- Run the State with s and get the value that results.

eval' :: State s a -> s -> a
eval' sa = fst . runState sa

-- Prelude> eval' get' "bunnicula"
-- "bunnicula"
-- Prelude> eval' get' "stake a bunny"
-- "stake a bunny"

-- 5
--Write a function which applies a function to create a new State.

modify'' :: (s -> s) -> State s () -- Since modify' is already there.
modify'' f = state $ \s -> ((),  f s)

-- Should behave like the following:
-- Prelude> runState (modify'' (+1)) 0
-- ((),1)
-- Prelude> runState (modify'' (+1) >> modify'' (+1)) 0
-- ((),2)
