-- | advent of code 2018-1

import qualified Data.Set as Set

type Freqs = Set.Set Integer

solve' :: Freqs -> Integer -> [Integer] -> Either (Freqs, Integer) Integer
solve' hist prevFreq xs
  | null xs                = Left (hist, prevFreq)
  | freq `Set.member` hist = Right freq
  | otherwise              = solve' (Set.insert freq hist) freq (tail xs)
  where freq = prevFreq + head xs

solve :: Freqs -> Integer -> [Integer] -> Integer
solve hist freq xs = case solve' hist freq xs of
  Left (newHist, prevFreq) -> solve newHist prevFreq xs
  Right value              -> value

-- Read num that can start with a '+'
readNum :: String -> Integer
readNum ('+' : xs) = read xs
readNum xs         = read xs

main :: IO ()
main = interact $ show . solve (Set.fromList [0]) 0 . map readNum . lines
