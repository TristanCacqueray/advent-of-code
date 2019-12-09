{-# LANGUAGE OverloadedStrings #-}
-- | Total number of direct and indirect orbits

import qualified Data.Text  as T
import           Data.Tree
import           Test.HUnit

type Orbit = (String, String)

type Orbits = Tree String

-- convert input to list of Orbit (origin, orbiting object).
readOrbits :: [String] -> [Orbit]
readOrbits = map decode
  where decode s = case T.splitOn ")" . T.pack $ s of
                     [a,b] -> (T.unpack a, T.unpack b)
                     _     -> error ("Invalid orbit: " ++ s)

-- add an Orbit to the Orbits tree. return Nothing if origin is not in the tree.
addOrbit :: Orbits -> Orbit -> Maybe Orbits
addOrbit tree (root, object) =
  if rootExists tree root then Just (addOrbit' tree) else Nothing
  where rootExists :: Orbits -> String -> Bool
        rootExists (Node label xs) s
          | label == s = True
          | otherwise  = foldl (\r t -> r || rootExists t s) False xs
        addOrbit' :: Orbits -> Orbits
        addOrbit' (Node label xs)
          | label == root = Node label (Node object []:xs)
          | otherwise = Node label (map addOrbit' xs)

-- convert list of Orbit to an Orbits tree.
buildOrbits :: [Orbit] -> Orbits
buildOrbits xs = buildOrbits' 0 (Node "COM" []) xs []
  where buildOrbits' :: Integer -> Orbits -> [Orbit] -> [Orbit] -> Orbits
        buildOrbits' _ tree [] [] = tree
        -- keep on trying until all Orbit are added
        buildOrbits' count tree [] remaining = if count > toInteger (length xs)
                                               then error ("Couldn't build tree.\n" ++
                                                           "Current: " ++ drawTree tree ++
                                                           "\nRemaining: " ++ show remaining)
                                               else buildOrbits' (count + 1) tree remaining []
        buildOrbits' c tree (x:cs) remaining = case addOrbit tree x of
          Just newTree -> buildOrbits' c newTree cs remaining
          Nothing      -> buildOrbits' c tree cs (x:remaining)

-- build a list of (object, number of steps) to reach an object.
buildList :: String -> Orbits -> [(String, Integer)]
buildList object = buildList' 0 []
  where buildList' :: Integer -> [(String, Integer)] -> Orbits -> [(String, Integer)]
        buildList' distance paths (Node label xs)
          | label == object = (label, distance):paths
          | null xs         = []
          | otherwise = case filter (not . null) (map (buildList' (distance + 1) ((label, distance):paths)) xs) of
              [] -> []
              ls -> head ls

-- measure the distance between two objects.
measureDistance :: String -> String -> Orbits -> Integer
measureDistance from to tree =
  measureDistances' fromList toList
  where fromList = buildList from tree
        fromPos = snd . head $ fromList
        toList = buildList to tree
        toPos = snd . head $ toList
        -- find intersection between two lists and return the distance difference
        measureDistances' :: [(String, Integer)] -> [(String, Integer)] -> Integer
        measureDistances' (_:fs) [] = measureDistances' fs toList
        measureDistances' ((fName, fPos):fs) ((tName, tPos):ts)
          | fName == tName = (fromPos - fPos) + (toPos - tPos) - 2
          | otherwise = measureDistances' ((fName, fPos):fs) ts

testOrbits = [
    ("COM", "B")
  , ("B", "C")
  , ("C", "D")
  , ("D", "E")
  , ("E", "F")
  , ("B", "G")
  , ("G", "H")
  , ("D", "I")
  , ("E", "J")
  , ("J", "K")
  , ("K", "L")
  , ("K", "YOU")
  , ("I", "SAN")
  ]

testMeasureDistance = TestCase $ assertEqual "test" 4 (measureDistance "YOU" "SAN" $ buildOrbits testOrbits)

solve :: [String] -> Integer
solve = measureDistance "YOU" "SAN" . buildOrbits . readOrbits

main :: IO ()
main = interact $ show . solve . lines
