-- | Find noun and verb that match an evaluation

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List     as L
import qualified Data.Maybe    as M
import qualified Data.Text     as T

import qualified Control.Monad as C
import qualified Data.Foldable as F

firstJust :: [Maybe a] -> Maybe a
firstJust = C.join . F.find M.isJust

replaceVal :: Integer -> Integer -> [Integer] -> [Integer]
replaceVal pos val state = let (a, b) = splitAt (fromInteger pos) state
                           in a ++ [val] ++ drop 1 b

evaluateOp :: [Integer] -> [Integer] -> (Integer -> Integer -> Integer) -> [Integer]
evaluateOp (m:n:d:_) state op = let a = state !! fromInteger m
                                    b = state !! fromInteger n
                                in replaceVal d (op a b) state


evaluate :: Integer -> [Integer] -> Integer
evaluate pos state = let opCode = toInteger (state !! fromInteger pos)
                         (_, opArg)  = splitAt (fromInteger pos + 1) state
                     in case opCode of
                         1  -> evaluate (pos + 4) $ evaluateOp opArg state (+)
                         2  -> evaluate (pos + 4) $ evaluateOp opArg state (*)
                         99 -> head state
                         _  -> 42

splitNum :: String -> [Integer]
splitNum = map (read . T.unpack) . T.splitOn "," . T.pack

try :: Integer -> Integer -> [Integer] -> Maybe Integer
try noun verb state = case evaluate 0 ((replaceVal 1 noun . replaceVal 2 verb) state) of
  19690720 -> Just (100 * noun + verb)
  _        -> Nothing

solve :: [Integer] -> Integer
solve state = let values = [(i, j) | i <- [0..100], j <- [0..100]]
                  tests = map (\(noun, verb) -> try noun verb state) values
              in case firstJust tests of
                   Just a  -> a
                   Nothing -> 42

main :: IO ()
main = interact $ show . solve . splitNum
