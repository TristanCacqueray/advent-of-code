-- | Thermal Environment Supervision Terminal with jumps and tests

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
getVal 1 v _ = v
getVal x _ _ = error ("Invalid mode" ++ show x)

evaluateOp :: [(Integer, Integer)] -> Mem -> (Integer -> Integer -> Integer) -> Mem
evaluateOp (_:_:(1, _):_) _ _ = error "Invalid position mode output"
evaluateOp (m:n:d:_) state op = let a = uncurry getVal m state
                                    b = uncurry getVal n state
                                in replaceVal (snd d) (op a b) state
evaluateOp _ _ _ = error "Invalid op arguments"

evaluateComparaison :: [(Integer, Integer)] -> Mem -> (Integer -> Integer -> Bool) -> Mem
evaluateComparaison ((m,v):(n,w):(_,d):_) state op = case op (getVal m v state) (getVal n w state) of
  True  -> replaceVal d 1 state
  False -> replaceVal d 0 state
evaluateComparaison _ _ _ = error "Invalid comp op arguments"

evaluateJump :: Integer -> [(Integer, Integer)] -> (Integer -> Integer -> Bool) -> Mem -> Integer
evaluateJump pos ((m, v):(n, w):_) op state = case op (getVal m v state) 0 of
  True -> getVal n w state
  False -> pos + 3
evaluateJump _ _ _ _ = error "Invalid jump op arguments"

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
       -- Add
       1  -> evaluate input (pos + 4) (output, evaluateOp opArgs state (+))
       -- Mul
       2  -> evaluate input (pos + 4) (output, evaluateOp opArgs state (*))
       -- Input
       3  -> evaluate input (pos + 2) (output, replaceVal (head opArg) input state)
       -- Output
       4  -> evaluate input (pos + 2) (getVal m1 (head opArg) state : output, state)
       -- Jump if true
       5  -> evaluate input (evaluateJump pos opArgs (/=) state) (output, state)
       -- Jump if False
       6  -> evaluate input (evaluateJump pos opArgs (==) state) (output, state)
       -- Less than
       7  -> evaluate input (pos + 4) (output, evaluateComparaison opArgs state (<))
       -- Equals
       8  -> evaluate input (pos + 4) (output, evaluateComparaison opArgs state (==))
       -- Halt
       99 -> (output, state)
       -- Oops
       _  -> error ("Invalid Instruction '" ++ show opCode ++ "' at: " ++ show pos)

evaluateOutput :: Integer -> Mem -> Integer
evaluateOutput input state = let (output, _) = evaluate input 0 ([], state)
                             in head output

complexProg :: String
complexProg = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\
              \1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\
              \999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

testOutput = TestList $ map (\(name, output, input, prog) -> TestCase $
                            assertEqual name output (evaluateOutput input (splitNum prog))) [
    ("EqOkPos", 1,  8, "3,9,8,9,10,9,4,9,99,-1,8")
  , ("EqKoPos", 0, 42, "3,9,8,9,10,9,4,9,99,-1,8")
  , ("LtOkPos", 1,  4, "3,9,7,9,10,9,4,9,99,-1,8")
  , ("LtKoPos", 0,  8, "3,9,7,9,10,9,4,9,99,-1,8")
  , ("EqOkIme", 1,  8, "3,3,1108,-1,8,3,4,3,99")
  , ("EqKoIme", 0,  5, "3,3,1108,-1,8,3,4,3,99")
  , ("LtOkIme", 1,  4, "3,3,1107,-1,8,3,4,3,99")
  , ("LtKoIme", 0,  8, "3,3,1107,-1,8,3,4,3,99")
  , ("JmOkPos", 0,  0, "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
  , ("JmKoPos", 1,  1, "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
  , ("JmOkIme", 0,  0, "3,3,1105,-1,9,1101,0,0,12,4,12,99,1")
  , ("JmKoIme", 1,  1, "3,3,1105,-1,9,1101,0,0,12,4,12,99,1")
  , ("Final1",   999, 4, complexProg)
  , ("Final2",  1000, 8, complexProg)
  , ("Final3",  1001, 10, complexProg)
  ]


solve :: Mem -> Integer
solve = evaluateOutput 5

splitNum :: String -> Mem
splitNum = map (read . T.unpack) . T.splitOn "," . T.pack

main :: IO ()
main = interact $ show . solve . splitNum
