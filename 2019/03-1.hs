-- | Find closest wire intersection
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text       as T
import qualified Data.Set        as S
import Test.HUnit

-- Convert puzzel inputs to a list of Segment
data Segment = Seg Integer Integer Integer
               deriving (Show, Eq)

testSegment = TestCase $ assertEqual "Test segment show" "Seg -1 2 3" (show $ Seg (-1) 2 3)

readSegments :: String -> [Segment]
readSegments = map (decode . T.unpack) . T.splitOn "," . T.pack
  where decode (d:n) = let v = read n :: Integer
                       in case d of 'L' -> Seg (-1)  0  v
                                    'U' -> Seg   0   1  v
                                    'R' -> Seg   1   0  v
                                    'D' -> Seg   0 (-1) v

testReadSegments = TestCase $ assertEqual "Test segment read"
                                         [Seg 1 0 3, Seg (-1) 0 42, Seg 0 1 99]
                                         (readSegments "R3,L42,U99")

-- Convert a list of Segment to a list of Point
data Point = P Integer Integer
             deriving (Show, Eq, Ord)

segmentToPoints :: Point -> Segment -> [Point]
segmentToPoints (P startX startY) (Seg x y c) = [P (startX + x * i) (startY + y * i) | i <- [1..c]]

testSegmentToPoints = TestCase $ assertEqual "Test segmentToPoints"
  [P 1 0, P 2 0] (segmentToPoints (P 0 0) (Seg 1 0 2))

segmentsToPoints :: Point -> [Segment] -> [Point]
segmentsToPoints startPoint = foldl fun [startPoint]
  where fun :: [Point] -> Segment -> [Point]
        fun points segment = points ++ segmentToPoints (last points) segment

testSegmentsToPoints = TestCase $ assertEqual "Test segmentsToPoints"
  [P 0 0, P 1 0, P 2 0, P 2 1, P 2 2, P 2 3]
  (segmentsToPoints (P 0 0) [Seg 1 0 2, Seg 0 1 3])

-- Find closest intersection
pointsIntersections :: [Point] -> [Point] -> [Point]
pointsIntersections l1 l2 = S.toList $ S.intersection (S.fromList l1) (S.fromList l2)

testPointsIntersections = TestCase $ assertEqual "Test pointsIntersections"
  [P 0 0, P 42 42]
  (pointsIntersections [P 0 0, P 1 2, P 3 3, P 42 42] [P 0 0, P 4 5, P 42 42])

taxiCab :: Point -> Integer
taxiCab (P x y) = abs x + abs y

closestDistance :: [Point] -> Integer
closestDistance = minimum . filter (\(n) -> n > 0) . map taxiCab

testClosestDistance = TestCase $ assertEqual "Test closestDistance" 2 (closestDistance [P 0 0, P 3 4, P 4 5, P (-1) 1])

-- Puzzle solution
solve :: [String] -> Integer
solve = findClosest . map ((segmentsToPoints (P 0 0)) . readSegments)
  where findClosest :: [[Point]] -> Integer
        findClosest (a:b:_) = closestDistance $ pointsIntersections a b

testSolve = TestCase $ assertEqual "Test solve" 159
  (solve ["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"])

testSolve2 = TestCase $ assertEqual "Test solve2" 135
  (solve ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])

main :: IO ()
main = interact $ show . solve . lines
