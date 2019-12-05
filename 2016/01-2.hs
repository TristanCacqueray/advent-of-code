-- | compute walk distance to first occurence
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List       as L
import qualified Data.Text       as T
import Test.HUnit

type Pos = (Integer, Integer)

data Dir = D Char Integer
           deriving (Show, Eq)
readDirs :: String -> [Dir]
readDirs = map (decode . T.unpack) . T.splitOn ", " . T.pack
  where decode (c:n) = D c (read n)

testReadDirs = TestCase $ assertEqual "Test readDirs" [D 'R' 42, D 'L' 44] (readDirs "R42, L44")

rotate (x, y) 'L' = (y * (-1), x)
rotate (x, y) 'R' = (y, x * (-1))

dirToPos :: Pos -> Pos -> Dir -> [Pos]
dirToPos (px, py) (dx, dy) (D c idx) = let (x, y) = rotate (dx, dy) c
                                       in [(px + x * i, py + y * i) | i <- [1..idx]]

firstPos :: [Pos] -> [Pos] -> Maybe Pos
firstPos _ [] = Nothing
firstPos hist (p:xs) = if p `elem` hist then Just p else firstPos hist xs

walkDirs :: [Dir] -> Pos
walkDirs = walkDirs' (0, 0) (0, 1) []
  where walkDirs' :: Pos -> Pos -> [Pos] -> [Dir] -> Pos
        walkDirs' pos dir hist ((D c idx):xs) =
          let newPos = dirToPos pos dir (D c idx)
              newHist = hist ++ newPos
              newDir = rotate dir c
              lastPos = last newPos
          in if null hist then walkDirs' lastPos newDir newHist xs
                          else case firstPos hist newPos of
                                 Just p  -> p
                                 Nothing -> walkDirs' lastPos newDir newHist xs

testWalkDirs = TestCase $ assertEqual "Test walkDirs" (4, 0) (walkDirs $ readDirs "R8, R4, R4, R8")

solve :: String -> Integer
solve s = abs x + abs y
  where (x, y) = walkDirs $ readDirs s

main :: IO ()
main = interact $ show . solve
