-- | Space Police

module Day11 where

import qualified Data.Set as Set
import           Day09    hiding (main, solveP1, solveP2)

data Color = Black | White
  deriving (Eq, Show)

type Hull = [(Int, Int, Color)]

getColor :: Hull -> (Int, Int) -> Color
getColor [] _ = Black
getColor ((hx, hy, c):xs) (x, y)
  | hx == x && hy == y = c
  | otherwise = getColor xs (x, y)

colorValue Black = 0
colorValue White = 1
colorCode 0 = Black
colorCode 1 = White
colorCode _ = error "Unknown color"
colorShow Black = ' '
colorShow White = 'X'

rotate :: (Int, Int) -> Integer -> (Int, Int)
rotate (x, y) 0 = (y * (-1), x)
rotate (x, y) 1 = (y, x * (-1))
rotate _ _      = error "Unknown direction"

paint :: Hull -> IntcodeComputer -> Hull
paint hull' = paint' hull' (0,0) (0,1)
  where paint' :: Hull -> (Int, Int) -> (Int, Int) -> IntcodeComputer -> Hull
        paint' hull (px, py) (dx, dy) computer =
          let curColor = colorValue $ getColor hull (px, py)
              computerColor = evaluateUntilOutput $ addInput curColor computer
              (color:_) = outputs computerColor
              computerDir = evaluateUntilOutput computerColor
              (dir:_) = outputs computerDir

              newDir = rotate (dx, dy) dir
              newPos = (px + fst newDir, py + snd newDir)
              newColor = colorCode color
          in case state computerColor of
            Halt -> hull
            _    -> paint' ((px, py, newColor):hull) newPos newDir computerDir


solveP1 :: IntcodeComputer -> String
solveP1 = show . length . Set.fromList . map (\(x, y, _) -> (x, y)) . paint []


-- Part2
showHull :: Hull -> [String]
showHull hull = showHull' maxX minY [] []
  where xs = map (\(x, _, _) -> x) hull
        ys = map (\(_, y, _) -> y) hull
        (minX, maxX) = (minimum xs, maximum xs)
        (minY, maxY) = (minimum ys, maximum ys)
        showHull' x y cur res
            | y > maxY = res
            | x <= minX = showHull' maxX (y + 1) [] (cur:res)
            | otherwise = showHull' (x - 1) y (colorShow (getColor hull (x, y)):cur) res

solveP2 :: IntcodeComputer -> String
solveP2 = unlines . showHull . paint [(0, 0, White)]

main :: IO ()
main = interact $ solveP2 . createComputer
