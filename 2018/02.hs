-- | https://adventofcode.com/2018/day/2

import           Data.List

type CharCount = [(Char, Int)]

groups :: String -> CharCount
groups s = map (\x -> (head x, length x)) $ group $ sort s

countFilter :: Int -> CharCount -> Bool
countFilter _ []          = False
countFilter i ((_, x):xs) = (x == i) || countFilter i xs

counts :: Int -> [CharCount] -> Integer
counts num xs = toInteger (length (filter (countFilter num) xs))

solve :: [CharCount] -> Integer
solve xs = counts 2 xs * counts 3 xs

main :: IO ()
main = interact $ show . solve . map groups . lines
