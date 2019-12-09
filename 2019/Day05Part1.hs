-- | Thermal Environment Supervision Terminal

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text     as T
import qualified Data.Char     as B
import Test.HUnit

type Mem = [Integer]
type Outputs = [Integer]

replaceVal :: Integer -> Integer -> Mem -> Mem
replaceVal pos val state = let (a, b) = splitAt (fromInteger pos) state
                           in a ++ [val] ++ drop 1 b

getVal :: Integer -> Integer -> Mem -> Integer
getVal 0 v state = state !! fromInteger v
getVal 1 v state = v

evaluateOp :: [(Integer, Integer)] -> Mem -> (Integer -> Integer -> Integer) -> Mem
evaluateOp (m:n:(1, _):_) _ _ = error "Invalid position mode output"
evaluateOp (m:n:d:_) state op = let a = getVal (fst m) (snd m) state
                                    b = getVal (fst n) (snd n) state
                                in replaceVal (snd d) (op a b) state


decodeOpCode :: Integer -> (Integer, Integer, Integer, Integer)
decodeOpCode x = decode' $ map (toInteger . B.digitToInt) (show x)
  where decode' (a:b:c:d:e:[]) = (a, b, c, (d * 10) + e)
        decode' (  b:c:d:e:[]) = (0, b, c, (d * 10) + e)
        decode' (    c:d:e:[]) = (0, 0, c, (d * 10) + e)
        decode' (      d:e:[]) = (0, 0, 0, (d * 10) + e)
        decode' (        e:[]) = (0, 0, 0, e)
        decode' _ = error ("Invalid OpCode: " ++ show x)


evaluate :: Integer -> Integer -> (Outputs, Mem) -> (Outputs, Mem)
evaluate input pos (output, state) =
  let (m3, m2, m1, opCode) = decodeOpCode (state !! fromInteger pos)
      (_, opArg) = splitAt (fromInteger pos + 1) state
      opArgs = zip [m1, m2, m3] opArg
  in case opCode of
       1  -> evaluate input (pos + 4) $ (output, evaluateOp opArgs state (+))
       2  -> evaluate input (pos + 4) $ (output, evaluateOp opArgs state (*))
       3  -> evaluate input (pos + 2) $ (output, replaceVal (head opArg) input state)
       4  -> evaluate input (pos + 2) $ ((getVal m1 (head opArg) state):output, state)
       99 -> (output, state)
       _  -> error ("Invalid Instruction '" ++ show opCode ++ "' at: " ++ show pos)

splitNum :: String -> Mem
splitNum = map (read . T.unpack) . T.splitOn "," . T.pack

testEvaluate = TestList [
    TestCase $ assertEqual "test" ([], [1002, 4, 3, 4, 99]) (evaluate 0 0 ([], (splitNum "1002,4,3,4,33")))
  , TestCase $ assertEqual "test" ([], [1101, 100, -1, 4, 99]) (evaluate 0 0 ([], (splitNum "1101,100,-1,4,0")))
  , TestCase $ assertEqual "outp" ([42], [4,3,99,42]) (evaluate 0 0 ([], (splitNum "4,3,99,42")))
  ]

solve :: Mem -> (Outputs, Mem)
solve state = evaluate 1 0 ([], state)

main :: IO ()
main = interact $ show . solve . splitNum
