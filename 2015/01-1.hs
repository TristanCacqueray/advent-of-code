-- | compute ( = +1 and ) = -1

import Test.HUnit

compute :: String -> Integer
compute = compute' 0
  where compute' idx []     = idx
        compute' idx ['\n'] = idx
        compute' idx (c:xs) = case c of
          '('  -> compute' (idx + 1) xs
          ')'  -> compute' (idx - 1) xs
          _ -> error ("Oops '" ++ [c] ++ "'")

testCompute :: Test
testCompute = TestList $ map (\(s, v) -> TestCase $ assertEqual ("Compute " ++ s) v (compute s))
  [("(())", 0), ("()()", 0), ("(((", 3), ("(()(()(", 3), ("))(((((", 3), ("))(", -1), (")())())", -3)]

main :: IO ()
main = interact $ show . compute
