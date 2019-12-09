-- | What is the highest signal that can be sent to the thrusters?
{-# LANGUAGE OverloadedStrings #-}
module Day07Part2 where

import           Day05Part2 hiding (evaluate, main, solve)
import qualified Day07Part1 as P1

data Result = NeedInput | Output | Halt
type InstructionPointer = Integer
type Amp = ([Integer], [Integer], InstructionPointer, Mem)

evaluate :: [Integer] -> InstructionPointer -> Mem -> (Mem, InstructionPointer, Result, Integer, [Integer])
evaluate input pos state =
  let (m3, m2, m1, opCode) = decodeOpCode (state !! fromInteger pos)
      (_, opArg) = splitAt (fromInteger pos + 1) state
      opArgs = zip [m1, m2, m3] opArg
  in case opCode of
       -- Add
       1  -> evaluate input (pos + 4) (evaluateOp opArgs state (+))
       -- Mul
       2  -> evaluate input (pos + 4) (evaluateOp opArgs state (*))
       -- Input
       3  -> case input of
         []   -> (state, pos + 2, NeedInput, 0, [])
         x:xs -> evaluate xs (pos + 2) (replaceVal (head opArg) x state)
       -- Output
       4  -> (state, (pos + 2), Output, (getVal m1 (head opArg) state), input)
       -- Jump if true
       5  -> evaluate input (evaluateJump pos opArgs (/=) state) state
       -- Jump if False
       6  -> evaluate input (evaluateJump pos opArgs (==) state) state
       -- Less than
       7  -> evaluate input (pos + 4) (evaluateComparaison opArgs state (<))
       -- Equals
       8  -> evaluate input (pos + 4) (evaluateComparaison opArgs state (==))
       -- Halt
       99 -> (state, pos, Halt, 0, input)
       -- Oops
       _  -> error ("Invalid Instruction '" ++ show opCode ++ "' at: " ++ show pos)


evaluateSettings :: [Amp] -> Integer
evaluateSettings amps = evaluateSettings' amps [] 0
  where evaluateSettings' :: [Amp] -> [Amp] -> Integer -> Integer
        evaluateSettings' []     remainings lastOutput = evaluateSettings' remainings [] lastOutput
        evaluateSettings' ((inputs, outputs, pos, state):xs) remainings lastOutput =
          case evaluate (inputs ++ [lastOutput]) pos state of
            (_, _, NeedInput, _, _)                       -> error "Missing inputs"
            (_, _, Halt, _, _)                            ->
              let lastStage = if null xs then (inputs, outputs, pos, state) else (last xs)
                  (_, lastOutputs, _, _) = lastStage
              in head lastOutputs
            (newState, newPos, Output, output, newInputs) ->
              evaluateSettings' xs (remainings ++ [(newInputs, output:outputs, newPos, newState)]) output

solve :: Mem -> Integer
solve state = let settings = [[([a], [], 0, state), ([b], [], 0, state), ([c], [], 0, state), ([d], [], 0, state),
                               ([e], [], 0, state)] |
                               a <- [5..9], b <- [5..9], c <- [5..9], d <- [5..9], e <- [5..9],
                               not . P1.hasDuplicates $ [a, b, c, d, e]]
              in maximum $ map evaluateSettings settings

main :: IO ()
main = interact $ show . solve . splitNum
