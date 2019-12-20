-- |

module Day03 where

import qualified Data.Set as Set

type House = (Integer, Integer)

type Direction = (Integer, Integer)

walkDirections :: [Direction] -> [House]
walkDirections xs = (0, 0) : walkDirections' (0, 0) xs
  where walkDirections' _ [] = []
        walkDirections' (x, y) ((dx, dy):restDirections) =
          let newPos = (x + dx, y + dy)
          in newPos : walkDirections' newPos restDirections

countUnique :: [House] -> Integer
countUnique = toInteger . length . Set.fromList

splitDir :: [Direction] -> [[Direction]]
splitDir = foldl splitDir' [[], []]
  where splitDir' :: [[Direction]] -> Direction -> [[Direction]]
        splitDir' (santa:robot:[]) direction
          | length santa /= length robot = [direction:santa,robot]
          | otherwise = [santa,direction:robot]


readDirections :: String -> [Direction]
readDirections []        = []
readDirections ('\n':[]) = []
readDirections ('\n':xs) = readDirections xs
readDirections ('^':xs)  = (1,  0):readDirections xs
readDirections ('<':xs)  = (0, -1):readDirections xs
readDirections ('>':xs)  = (0,  1):readDirections xs
readDirections ('v':xs)  = (-1, 0):readDirections xs
readDirections _         = error "Invalid direction"

solveP2 s = let dirs = splitDir $ readDirections s
                santa = reverse $ dirs !! 0
                robot = reverse $ dirs !! 1
            in countUnique $ (walkDirections santa) ++ (walkDirections robot)

solveP1 = countUnique . walkDirections . readDirections

main = interact $ show . solveP2
