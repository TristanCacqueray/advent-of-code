{-# LANGUAGE OverloadedStrings #-}
-- | Sensor Boost: What BOOST keycode does it produce?

module Day09 where

import           Test.HUnit

import qualified Data.Char  as C
import qualified Data.Text  as T

type Mem = [Integer]

data ComputerState = Running | NeedInput | Output | Halt
             deriving (Show, Eq)

data IntcodeComputer = Computer
  { eix      :: Integer
  , rel_base :: Integer
  , memory   :: Mem
  , inputs   :: [Integer]
  , outputs  :: [Integer]
  , state    :: ComputerState
  }
  deriving (Show, Eq)

data ArgMode = Position | Immediate | Relative
  deriving (Show, Eq)

type OpArgs = (ArgMode, Integer)

moveEix :: Integer -> IntcodeComputer -> IntcodeComputer
moveEix x computer = computer { eix = eix computer + x }

getMemValue :: Mem -> Integer -> Integer
getMemValue mem pos
  | fromInteger pos >= length mem = 0
  | pos < 0                       = error ("Access to negative memory" ++ show pos)
  | otherwise                     = mem !! fromInteger pos

setMemValue :: Integer -> Integer -> Mem -> Mem
setMemValue pos val mem =
   let newMem = if fromInteger pos >= length mem
                then mem ++ [0 | _ <- [0..(fromInteger pos - length mem)]] else mem
       (x, y) = splitAt (fromInteger pos) newMem
   in x ++ [val] ++ drop 1 y

getValue :: ArgMode -> Integer -> IntcodeComputer -> Integer
getValue Position v computer = getMemValue (memory computer) v
getValue Immediate v _       = v
getValue Relative v computer = getMemValue (memory computer) (rel_base computer + v)

getAddr :: ArgMode -> Integer -> IntcodeComputer -> Integer
getAddr Relative v computer = (rel_base computer + v)
getAddr _ v _               = v

evaluateOp :: [OpArgs] -> (Integer -> Integer -> Integer) -> IntcodeComputer -> IntcodeComputer
evaluateOp (_:_:(Immediate, _):_) _ _ = error "Output mode can not be Immediate"
evaluateOp (m:n:o:_) op computer = let a = uncurry getValue m computer
                                       b = uncurry getValue n computer
                                       c = uncurry getAddr o computer
                                   in moveEix 4 (computer { memory = setMemValue c (op a b) (memory computer) })
evaluateOp _ _ _                      = error "Invalid op arguments"

evaluateJump :: [OpArgs] -> (Integer -> Integer -> Bool) -> IntcodeComputer -> IntcodeComputer
evaluateJump ((m, v):(n, w):_) op computer =
  let result = if op (getValue m v computer) 0 then getValue n w computer - eix computer else 3
  in moveEix result computer
evaluateJump _ _ _                         = error "Invalid jump op arguments"

evaluateComparaison :: [OpArgs] -> (Integer -> Integer -> Bool) -> IntcodeComputer -> IntcodeComputer
evaluateComparaison ((m,v):(n,w):(o,d):_) op computer =
  let result = if op (getValue m v computer) (getValue n w computer) then 1 else 0
      addr = getAddr o d computer
  in moveEix 4 computer { memory = setMemValue addr result (memory computer) }
evaluateComparaison _ _ _ = error "Invalid comp op arguments"

evaluateInput :: [OpArgs] -> IntcodeComputer -> IntcodeComputer
evaluateInput (m:_) computer = case inputs computer of
          []   -> computer { state = NeedInput }
          x:xs -> moveEix 2 computer { memory = setMemValue (uncurry getAddr m computer) x (memory computer),
                                       inputs = xs }
evaluateInput _ _            = error "Invalid input arguments"

decodeOpCode :: Integer -> (ArgMode, ArgMode, ArgMode, Integer)
decodeOpCode x = decode' $ map (toInteger . C.digitToInt) (show x)
  where argMode :: Integer -> ArgMode
        argMode 0 = Position
        argMode 1 = Immediate
        argMode 2 = Relative
        argMode m = error ("Invalid arg mode" ++ show m)
        decode' (a:b:c:d:e:[]) = (argMode a, argMode b, argMode c, (d * 10) + e)
        decode' (  b:c:d:e:[]) = (argMode 0, argMode b, argMode c, (d * 10) + e)
        decode' (    c:d:e:[]) = (argMode 0, argMode 0, argMode c, (d * 10) + e)
        decode' (      d:e:[]) = (argMode 0, argMode 0, argMode 0, (d * 10) + e)
        decode' (        e:[]) = (argMode 0, argMode 0, argMode 0, e)
        decode' _              = error ("Invalid OpCode: " ++ show x)

addInput :: Integer -> IntcodeComputer -> IntcodeComputer
addInput input computer = computer { inputs = input:inputs computer }

addOutput :: Integer -> IntcodeComputer -> IntcodeComputer
addOutput output computer = computer { outputs = output:outputs computer }

evaluate :: IntcodeComputer -> IntcodeComputer
evaluate computer =
     let (m3, m2, m1, opCode) = decodeOpCode (getMemValue (memory computer) (eix computer))
         (_, opArg) = splitAt (fromInteger (eix computer) + 1) (memory computer)
         opArgs = zip [m1, m2, m3] opArg
     in case opCode of
        -- Add
        1 -> evaluateOp opArgs (+) computer
        -- Mul
        2 -> evaluateOp opArgs (*) computer
        -- Input
        3 -> evaluateInput opArgs computer
        -- Output
        4  -> moveEix 2 (addOutput (getValue m1 (head opArg) computer) computer)
        -- Jump if true
        5  -> evaluateJump opArgs (/=) computer
        -- Jump if False
        6  -> evaluateJump opArgs (==) computer
        -- Less than
        7  -> evaluateComparaison opArgs (<) computer
        -- Equals
        8  -> evaluateComparaison opArgs (==) computer
        -- Change rel base
        9  -> moveEix 2 computer { rel_base = rel_base computer + getValue m1 (head opArg) computer }
        -- Halt
        99 -> moveEix 1 computer { state = Halt }
        -- Oops
        _  -> error ("Invalid Instruction '" ++ show opCode ++ "' at: " ++ show computer)

evaluateUntilHalt :: IntcodeComputer -> IntcodeComputer
evaluateUntilHalt computer = let next = evaluate computer
                             in case state next of
                                  Halt      -> next
                                  NeedInput -> error "Need input"
                                  _         -> evaluateUntilHalt next

createComputer :: String -> IntcodeComputer
createComputer s = let mem = map (read . T.unpack) . T.splitOn "," $ T.pack s
                   in Computer { eix = 0, rel_base = 0, memory = mem, state = Running, inputs = [], outputs = [] }

longCode = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

quineCode = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"

testCode :: Test
testCode = TestList $ [
    TestCase $ assertEqual "test1" [0] (output 0 "3,9,8,9,10,9,4,9,99,-1,8")
  , TestCase $ assertEqual "test2" [1] (output 8 "3,9,8,9,10,9,4,9,99,-1,8")
  , TestCase $ assertEqual "test3" [1] (output 0 "3,9,7,9,10,9,4,9,99,-1,8")
  , TestCase $ assertEqual "test4" [1] (output 8 "3,3,1108,-1,8,3,4,3,99")
  , TestCase $ assertEqual "test5" [0] (output 42 "3,3,1107,-1,8,3,4,3,99")
  , TestCase $ assertEqual "test6" [0] (output 0  "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
  , TestCase $ assertEqual "test7" [1] (output 42 "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
  , TestCase $ assertEqual "test8" [0] (output 0  "3,3,1105,-1,9,1101,0,0,12,4,12,99,1")
  , TestCase $ assertEqual "test9" [1] (output 42 "3,3,1105,-1,9,1101,0,0,12,4,12,99,1")
  , TestCase $ assertEqual "test10" [999] (output 4 longCode)
  , TestCase $ assertEqual "test11" [1000] (output 8 longCode)
  , TestCase $ assertEqual "test12" [1001] (output 42 longCode)
  , TestCase $ assertEqual "test13" [99,0,101,1006,101,16,100,1008,100,1,100,1001,-1,204,1,109]
    (outputs (evaluateUntilHalt $ createComputer quineCode))
  , TestCase $ assertEqual "test14" [1219070632396864]
    (outputs (evaluateUntilHalt $ createComputer "1102,34915192,34915192,7,4,7,99,0"))
  , TestCase $ assertEqual "test15" [1125899906842624]
    (outputs (evaluateUntilHalt $ createComputer "104,1125899906842624,99"))
  ]
  where output input code = (outputs (evaluateUntilHalt $ addInput input $ createComputer code))

solver :: Integer -> String -> (ComputerState, [Integer])
solver i s = let computer = evaluateUntilHalt $ addInput i $ createComputer s
             in (state computer, outputs computer)

solveP1 = solver 1
solveP2 = solver 2

main :: IO ()
main = interact $ show . solveP2
