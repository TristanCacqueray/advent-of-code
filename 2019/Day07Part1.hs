-- | What is the highest signal that can be sent to the thrusters?
{-# LANGUAGE OverloadedStrings #-}
module Day07Part1 where

import qualified Data.Set   as Set
import           Day05Part2 hiding (main, solve)

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list

evaluateSettings :: Mem -> [Integer] -> Integer
evaluateSettings state = evaluateSettings' state 0
  where evaluateSettings' :: Mem -> Integer -> [Integer] -> Integer
        evaluateSettings' _ input [] = input
        evaluateSettings' curState input (x:xs) =
          let ([o], newState) = evaluate [x, input] 0 ([], curState)
          in evaluateSettings' newState o xs

solve :: Mem -> Integer
solve state = let settings = [[a, b, c, d, e] | a <- [0..4], b <- [0..4], c <- [0..4], d <- [0..4], e <- [0..4]]
              in maximum $ map (evaluateSettings state) $ filter (not . hasDuplicates) settings

main :: IO ()
main = interact $ show . solve . splitNum
