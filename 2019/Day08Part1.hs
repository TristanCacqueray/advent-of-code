-- | Check Space Image Format

module Day08Part1 where

import qualified Data.Char as C
import qualified Data.List as L

type Layer = [Integer]
type Dimension = (Integer, Integer)
type Image = (Dimension, [Layer])

readImage :: Dimension -> [Integer] -> Image
readImage (x, y) = readImage' []
  where readImage' :: [Layer] -> [Integer] -> Image
        readImage' layers [] = ((x, y), reverse layers)
        readImage' layers xs = let newLayer = take (fromInteger (x * y)) xs
                                   rest = drop (fromInteger (x * y)) xs
                               in readImage' (newLayer:layers) rest

convertInput :: [Char] -> [Integer]
convertInput = convertInput' []
  where convertInput' :: [Integer] -> [Char] -> [Integer]
        convertInput' result []     = reverse $ filter (\a -> a >= 0) result
        convertInput' result (x:xs) = convertInput' (readChar x:result) xs
        readChar :: Char -> Integer
        readChar x = toInteger $ C.ord x - 48

getNumCount :: Integer -> Layer -> Integer
getNumCount v l = getNumCount' $ map (\l -> (length l, head l)) $ L.group $ L.sort l
  where getNumCount' :: [(Int, Integer)] -> Integer
        getNumCount' [] = 0
        getNumCount' ((count, value):xs) = if value == v then toInteger count else getNumCount' xs

check :: Image -> Integer
check (_, layers) =
  let minLayer = head $ L.sortBy (\a b -> compare (getNumCount 0 a) (getNumCount 0 b))
                      $ filter (\a -> getNumCount 0 a > 0) layers
  in getNumCount 1 minLayer * getNumCount 2 minLayer

solve :: String -> Integer
solve = check . readImage (25, 6) . convertInput

main :: IO ()
main = interact $ show . solve
