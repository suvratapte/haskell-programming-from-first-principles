 -- I don't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}

module Chapter_23 where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad.Trans.State
import Control.Monad (replicateM)
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
