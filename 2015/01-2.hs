-- | get index of basement step

import Test.HUnit

compute :: String -> Integer
compute = compute' 0 0
  where compute' (-1) idx _      = idx
        compute' _      _ ['\n'] = error "Basement not found"
        compute' pos  idx (c:xs) = case c of
          '('  -> compute' (pos + 1) (idx + 1) xs
          ')'  -> compute' (pos - 1) (idx + 1) xs
          _    -> error ("Oops '" ++ [c] ++ "'")

testCompute :: Test
testCompute = TestList $ map (\(s, v) -> TestCase $ assertEqual ("Compute " ++ s) v (compute s))
  [(")", 1), ("()())", 5)]

main :: IO ()
main = interact $ show . compute
