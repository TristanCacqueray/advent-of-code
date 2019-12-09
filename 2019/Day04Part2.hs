-- | Find possible count between range

import qualified Data.Char       as C
import qualified Data.List       as L
import Test.HUnit

valid :: Integer -> Bool
valid x = let list = map C.digitToInt (show x)
          in testInc list && testGroups list
             where testInc (a:b:c:d:e:f:[]) = b >= a && c >= b && d >= c && e >= d && f >= e &&
                                            ( a == b || b == c || c == d || d == e || e == f )
                   testGroups = foldl (\b a -> b || length a == 2) False . L.group

testValid = TestList $ map (\x -> TestCase $ assertEqual ("Test " ++ show (fst x)) (snd x) (valid (fst x)))
  [(111111, False), (112233,  True), (111123, False), (111122,  True),
   (123444, False), (135679, False), (223450, False), (123789, False)]

inc :: Integer -> Integer
inc x = let y = x + 1
        in if valid y then y else inc y

count :: Integer -> Integer -> Integer -> Integer
count limit current idx
  | current > limit  = idx - 1
  | current == limit = idx
  | otherwise        = count limit (inc current) (idx + 1)

testCount = TestCase $ assertEqual "Test count" 1172 (count 630395 153517 0)
