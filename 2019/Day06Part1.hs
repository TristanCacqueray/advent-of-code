{-# LANGUAGE OverloadedStrings #-}
-- | Total number of direct and indirect orbits

import qualified Data.Text  as T
import           Data.Tree
import           Test.HUnit

type Orbit = (String, String)

type Orbits = Tree String

readOrbits :: [String] -> [Orbit]
readOrbits = map decode
  where decode s = case T.splitOn ")" . T.pack $ s of
                     [a,b] -> (T.unpack a, T.unpack b)
                     _     -> error ("Invalid orbit: " ++ s)

addOrbit :: Orbits -> Orbit -> Maybe Orbits
addOrbit tree (root, object) =
  if rootExists tree root then Just (addOrbit' tree) else Nothing
  where rootExists :: Orbits -> String -> Bool
        rootExists (Node label xs) s
          | label == s = True
          | otherwise  = foldl (\r t -> r || rootExists t s) False xs
        addOrbit' :: Orbits -> Orbits
        addOrbit' (Node label xs)
          | label == root = Node label ((Node object []):xs)
          | otherwise = (Node label (map addOrbit' xs))


buildOrbits :: [Orbit] -> Orbits
buildOrbits xs = buildOrbits' 0 (Node "COM" []) xs []
  where buildOrbits' :: Integer -> Orbits -> [Orbit] -> [Orbit] -> Orbits
        buildOrbits' _ tree [] [] = tree
        buildOrbits' count tree [] remaining = if count > toInteger (length xs)
                                               then error ("Couldn't build tree.\n" ++
                                                           "Current: " ++ drawTree tree ++
                                                           "\nRemaining: " ++ show remaining)
                                               else buildOrbits' (count + 1) tree remaining []
        buildOrbits' c tree (x:cs) remaining = case addOrbit tree x of
          Just newTree -> buildOrbits' c newTree cs remaining
          Nothing      -> buildOrbits' c tree cs (x:remaining)

countOrbits :: Orbits -> Integer
countOrbits = countOrbits' 0
  where countOrbits' distance (Node _ xs) = distance + foldl (\acc e -> acc + countOrbits' (distance + 1) e) 0 xs

testOrbits = [
   ("COM", "B"), ("B", "C"), ("C", "D"), ("D", "E"), ("E", "F"), ("B", "G"),
   ("G", "H"), ("D", "I"), ("E", "J"), ("J", "K"), ("K", "L")
   ]
testCountOrbits = TestCase $ assertEqual "test" 42 (countOrbits $ buildOrbits testOrbits)

solve :: [String] -> Integer
solve = countOrbits . buildOrbits . readOrbits

main :: IO ()
main = interact $ show . solve . lines
