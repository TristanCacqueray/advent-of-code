-- | compute walk distance
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List       as L
import qualified Data.Text       as T
import Test.HUnit

data Dir = D Char Integer
           deriving (Show, Eq)
readDirs :: String -> [Dir]
readDirs = map (decode . T.unpack) . T.splitOn ", " . T.pack
  where decode (c:n) = D c (read n)

testReadDirs = TestCase $ assertEqual "Test readDirs" [D 'R' 42, D 'L' 44] (readDirs "R42, L44")

rotate (x, y) 'L' = (y * (-1), x)
rotate (x, y) 'R' = (y, x * (-1))

walkDirs :: [Dir] -> (Integer, Integer)
walkDirs = walkDirs' (0, 0) (0, 1)
  where walkDirs' pos dir [] = pos
        walkDirs' (px, py) (dx, dy) ((D c idx):xs) = let (x, y) = rotate (dx, dy) c
                                                     in walkDirs' (px + x * idx, py + y * idx) (x, y) xs

testWalkDirs = TestList [
    TestCase $ assertEqual "Test walkDirs" (2, 3) (walkDirs $ readDirs "R2, L3")
  , TestCase $ assertEqual "Test walkDirs" (0, -2) (walkDirs $ readDirs "R2, R2, R2")
  ]

solve :: String -> Integer
solve s = abs x + abs y
  where (x, y) = walkDirs $ readDirs s

testSolve = TestCase $ assertEqual "Test solve" 12 (solve "R5, L5, R5, R3")

main :: IO ()
main = interact $ show . solve
