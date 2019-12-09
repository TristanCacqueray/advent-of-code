-- | Find closest wire intersection using steps count
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List       as L
import qualified Data.Text       as T
import qualified Data.Set        as S
import qualified Data.Maybe    as M
import Test.HUnit

-- Convert puzzel inputs to a list of Segment
data Segment = Seg Integer Integer Integer
               deriving (Show, Eq)

readSegments :: String -> [Segment]
readSegments = map (decode . T.unpack) . T.splitOn "," . T.pack
  where decode (d:n) = let v = read n :: Integer
                       in case d of 'L' -> Seg (-1)  0  v
                                    'U' -> Seg   0   1  v
                                    'R' -> Seg   1   0  v
                                    'D' -> Seg   0 (-1) v

-- Convert a list of Segment to a list of Point
data Point = P Integer Integer
             deriving (Show, Eq, Ord)

segmentToPoints :: Point -> Segment -> [Point]
segmentToPoints (P startX startY) (Seg x y c) = [P (startX + x * i) (startY + y * i) | i <- [1..c]]

segmentsToPoints :: Point -> [Segment] -> [Point]
segmentsToPoints startPoint = foldl fun [startPoint]
  where fun :: [Point] -> Segment -> [Point]
        fun points segment = points ++ segmentToPoints (last points) segment

-- Find closest intersection
pointsIntersections :: [Point] -> [Point] -> [Point]
pointsIntersections l1 l2 = S.toList $ S.intersection (S.fromList l1) (S.fromList l2)

stepDistance :: Point -> [[Point]] -> Integer
stepDistance point = sum . map (toInteger . (M.fromMaybe 42) . (L.elemIndex point))

testStepDistance = TestCase $ assertEqual "Test stepDistance" 3
  (stepDistance (P 1 1) [[P 0 0, P 1 1, P 42 42], [P 0 0, P 0 1, P 1 1]])

closestDistance :: [[Point]] -> [Point] -> Integer
closestDistance lists = minimum . filter (\(n) -> n > 0) . map ((flip stepDistance) lists)

findClosestDistance :: [Point] -> [Point] -> Integer
findClosestDistance l1 l2 = closestDistance [l1, l2] (pointsIntersections l1 l2)

testFindClosestDistance = TestCase $ assertEqual "Test findClosestDistance"
  3 (findClosestDistance [P 0 0, P 3 4] [P 0 0, P 1 2, P 3 4])

-- Puzzle solution
solve :: [String] -> Integer
solve = findClosest . map ((segmentsToPoints (P 0 0)) . readSegments)
   where findClosest :: [[Point]] -> Integer
         findClosest (a:b:_) = findClosestDistance a b

testSolve = TestCase $ assertEqual "Test solve" 610
   (solve ["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"])

testSolve2 = TestCase $ assertEqual "Test solve2" 410
   (solve ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])

main :: IO ()
main = interact $ show . solve . lines
