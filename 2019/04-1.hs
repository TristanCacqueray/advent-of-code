-- | Find possible count between range

import qualified Data.Char       as C
import Test.HUnit

valid :: Integer -> Bool
valid x = test $ map C.digitToInt (show x)
  where test (a:b:c:d:e:f:[]) = b >= a && c >= b && d >= c && e >= d && f >= e &&
                              ( a == b || b == c || c == d || d == e || e == f )

testValid = TestList [
  TestCase $ assertEqual "Test Valid" True  (valid 111111),
  TestCase $ assertEqual "Test Valid" True  (valid 111123),
  TestCase $ assertEqual "Test Valid" False (valid 135679),
  TestCase $ assertEqual "Test Valid" False (valid 223450),
  TestCase $ assertEqual "Test Valid" False (valid 123789)
  ]

inc :: Integer -> Integer
inc x = let y = x + 1
        in if valid y then y else inc y

count :: Integer -> Integer -> Integer -> Integer
count limit current idx
  | current > limit  = idx - 1
  | current == limit = idx
  | otherwise        = count limit (inc current) (idx + 1)

testCount = TestCase $ assertEqual "Test count" 1729 (count 630395 153517 0)
