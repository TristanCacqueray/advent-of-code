{-# LANGUAGE OverloadedStrings #-}
-- | I Was Told There Would Be No Math

module Day02Part01 where

import qualified Data.Text as T
type Box = (Integer, Integer, Integer)

wrappingSurface :: Box -> Integer
wrappingSurface (l, w, h) =
  let surfaces = [2*l*w, 2*w*h, 2*h*l]
      slack = minimum surfaces `div` 2
  in sum surfaces + slack

ribbonLength :: Box -> Integer
ribbonLength (l, w, h) =
  let perimeters = [l*2 + w*2, w*2 + h*2, l*2 + h*2]
      minPerimeters = minimum perimeters
  in minPerimeters + (l * w * h)

readBox :: String -> Box
readBox s = readBox' (map (read . T.unpack) (T.splitOn (T.pack "x") (T.pack s)))
  where readBox' :: [Integer] -> Box
        readBox' [a,b,c] = (a, b, c)
        readBox' _       = error "Box requires 3 dimensions"

solver :: (Box -> Integer) -> String -> Integer
solver op = sum . map (op . readBox) . lines

solveP2 = solver ribbonLength

solveP1 = solver wrappingSurface

main :: IO ()
main = interact $ show . solveP2
