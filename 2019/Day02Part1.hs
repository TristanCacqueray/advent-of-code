-- | Intcode program (three opcode vm add mul and halt)
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as L
import qualified Data.Text as T

replaceVal :: Integer -> Integer -> [Integer] -> [Integer]
replaceVal pos val state = let (a, b) = splitAt (fromInteger pos) state
                           in a ++ [val] ++ drop 1 b

evaluateOp :: [Integer] -> [Integer] -> (Integer -> Integer -> Integer) -> [Integer]
evaluateOp (m:n:d:_) state op = let a = state !! fromInteger m
                                    b = state !! fromInteger n
                                in replaceVal d (op a b) state


evaluate :: Integer -> [Integer] -> [Integer]
evaluate pos state = let opCode = toInteger (state !! fromInteger pos)
                         (_, opArg)  = splitAt (fromInteger pos + 1) state
                     in case opCode of
                         1  -> evaluate (pos + 4) $ evaluateOp opArg state (+)
                         2  -> evaluate (pos + 4) $ evaluateOp opArg state (*)
                         99 -> state
                         _  -> [42]

splitNum :: String -> [Integer]
splitNum = map (read . T.unpack) . T.splitOn "," . T.pack

joinNum :: [Integer] -> String
joinNum = L.intercalate "," . map show

fixProgram :: [Integer] -> [Integer]
fixProgram = replaceVal 1 12 . replaceVal 2 2

main :: IO ()
main = interact $ joinNum . evaluate 0 . fixProgram . splitNum
