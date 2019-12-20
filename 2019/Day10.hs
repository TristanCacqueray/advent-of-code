-- | Monitoring Station
{-# LANGUAGE OverloadedStrings #-}
module Day10 where

import qualified Data.List  as List
import qualified Data.Set   as Set
import           Test.HUnit

type Vector = (Integer, Integer)
type Asteroid = (Integer, Integer)

readMap :: [String] -> [Asteroid]
readMap = readMap' 0
  where readMap' _ []     = []
        readMap' y (s:ss) = (readLine 0 s) ++ (readMap' (y + 1) ss)
          where readLine x [] = []
                readLine x (c:cs) =
                  case c of
                    '#' -> (x, y):(readLine (x+1) cs)
                    _   -> readLine (x+1) cs

firstMap = [
    "o#oo#"
  , "ooooo"
  , "#####"
  , "oooo#"
  , "ooo##"
  ]

norm :: Vector -> Vector
norm (x, y) = let c = gcd x y
              in (x `div` c, y `div` c)

getVectors :: Asteroid -> [Asteroid] -> [Vector]
getVectors _ [] = []
getVectors source (x:xs)
  | x == source = getVectors source xs
  | otherwise   = (vector source x):(getVectors source xs)
  where vector :: Asteroid -> Asteroid -> Vector
        vector (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

countVisible :: Asteroid -> [Asteroid] -> Integer
countVisible source = toInteger . length . Set.fromList . map norm . (getVectors source)

testCountVisible :: Test
testCountVisible = TestList [
    TestCase $ assertEqual "test1" [(1,0),(2,0),(3,0)] (readMap ["o###"])
  , TestCase $ assertEqual "test2" 1 (count (0,0) ["o###"])
  , TestCase $ assertEqual "test3" 3 (count (0,1) ["o###"])
  , TestCase $ assertEqual "test4" 3 (count (0,1) ["o###"])
  , TestCase $ assertEqual "test5" 4 (count (1,1) ["o#o", "###", "o#o", "o#o"])
  ]
  where count x m = countVisible x $ readMap m


solveP1 :: [Asteroid] -> Integer
solveP1 list = maximum $ solveP1' list
  where solveP1' []     = []
        solveP1' (x:xs) = countVisible x list : solveP1' xs

testP1 :: Test
testP1 = TestCase $ assertEqual "test" 8 (solveP1 (readMap firstMap))

-- Part2
type Slop = (Int, Float)
type Targets = [(Slop, [Asteroid])]

lazerPos :: [Asteroid] -> Asteroid
lazerPos l = snd $ maximum $ countVisibles l
  where countVisibles []     = []
        countVisibles (x:xs) = (countVisible x l, x) : countVisibles xs

testLazerPos :: Test
testLazerPos = TestCase $ assertEqual "test" (3,4) (lazerPos (readMap firstMap))

getSlops :: Asteroid -> [Asteroid] -> [(Slop, Asteroid)]
getSlops _ []     = []
getSlops lazer (x:xs)
  | x == lazer = getSlops lazer xs
  | otherwise  = (slop lazer x, x): getSlops lazer xs
  where slop :: Asteroid -> Asteroid -> Slop
        slop (x1, y1) (x2, y2) =
          let sign = if x2 - x1 < 0 then 1 else -1
          in (sign, fromInteger (y2 - y1) / fromInteger (x2 - x1))

groupSlops :: [(Slop, Asteroid)] -> Targets
groupSlops = reverse . foldl groupSlops' [] . List.sort
  where groupSlops' [] (slop, asteroid) = [(slop, [asteroid])]
        groupSlops' acc (slop, asteroid) =
          let (lastSlop, asteroids):results = acc
          in if slop == lastSlop
             then (lastSlop, asteroid:asteroids) : results
             else (slop, [asteroid]) : acc

sortSlops :: Asteroid -> Targets -> Targets
sortSlops (lazerX, lazerY) = map (\(slop, asteroids) -> (slop, List.sortBy sortSlops' asteroids))
  where sortSlops' a b = compare (dist a) (dist b)
        dist (x, y) = sqrt $ fromInteger (lazerX - x) ** 2 + fromInteger (lazerY - y) ** 2

shoot :: Targets -> Targets
shoot ((_, []):xs)                    = shoot xs
shoot ((slop, [asteroid]):xs)         = xs
shoot ((slop, asteroid:asteroids):xs) = xs ++ [(slop, asteroids)]

shoots :: Integer -> Targets -> Asteroid
shoots count list = let (slop, asteroid:asteroids):xs = iterate shoot list !! fromInteger (count - 1)
                    in asteroid
--                    in (slop, asteroid:asteroids):xs

solveP2 count list =
  let lazer = lazerPos list
      rest = filter (\x -> x /= lazer) list
  in shoots count $ sortSlops lazer $ groupSlops $ getSlops lazer rest

main :: IO ()
main = interact $ show . solveP2 200 . readMap . lines

otherMap = [
    "o#o"
  , "o#o"
  , "###"
  , "o#o"
  , "o#o"
           ]

largeMap = [
    ".#..##.###...#######"
  , "##.############..##."
  , ".#.######.########.#"
  , ".###.#######.####.#."
  , "#####.##.#.##.###.##"
  , "..#####..#.#########"
  , "####################"
  , "#.####....###.#.#.##"
  , "##.#################"
  , "#####.##.###..####.."
  , "..######..##.#######"
  , "####.##.####...##..#"
  , ".#####..#.######.###"
  , "##...#.##########..."
  , "#.##########.#######"
  , ".####.#.###.###.#.##"
  , "....##.##.###..#####"
  , ".#.#.###########.###"
  , "#.#.#.#####.####.###"
  , "###.##.####.##.#..##"
  ]

testLargeMap = TestList $ map
  (\(count, res) -> TestCase $ assertEqual (show count) res (solveP2 count $ readMap largeMap)) [
    (1, (11, 12))
  , (2, (12, 1))
  , (3, (12, 2))
  , (10, (12,8))
  , (20, (16,0))
  , (50, (16,9))
  , (100, (10,16))
  , (199, (9,6))
  , (200, (8,2))
  , (201, (10,9))
  , (299, (11,1))
  ]
