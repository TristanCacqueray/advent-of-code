-- | take its mass, divide by three, round down, and subtract 2

requiredFuel :: Integer -> Integer
requiredFuel x = floor (fromIntegral x / 3) - 2

main :: IO ()
main = interact $ show . sum . map requiredFuel . map read . lines
