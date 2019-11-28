-- | advent of code 2018-1

-- Read num that can start with a '+'
readNum ('+' : xs) = read xs
readNum xs = read xs

main = interact $ show . sum . map readNum . lines
