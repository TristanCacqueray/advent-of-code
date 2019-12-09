-- | compute requiredFuel recursively

requiredFuel :: Integer -> Integer
requiredFuel x
  | x <= 0 = 0
  | otherwise = y + requiredFuel y
                where y = max 0 (floor (fromIntegral x / 3) - 2)

main :: IO ()
main = interact $ show . sum . map requiredFuel . map read . lines
