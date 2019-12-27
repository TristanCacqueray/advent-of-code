{-# LANGUAGE OverloadedStrings #-}
-- | The N-Body Problem

-- module Day12 where

import qualified Data.Set  as S
import qualified Data.Text as T

type Pos = (Integer, Integer, Integer)
type Velocity = (Integer, Integer, Integer)
type Moon = (Pos, Velocity)

createMoon :: String -> Moon
createMoon = createMoon' . split . trim
  where trim = T.pack . reverse . (++) "," . drop 1 . reverse . drop 1
        split = map split' . filter (\x -> ',' `elem` x) . map T.unpack . T.splitOn "="
        split' = read . T.unpack . head . T.splitOn "," . T.pack
        createMoon' (x:y:z:[]) = ((x, y, z), (0, 0, 0))
        createMoon' _          = error "Invalid moon"

readMoons :: [String] -> [Moon]
readMoons = map createMoon

gravity' a b
  | a < b     = 1
  | a == b    = 0
  | a > b     = (-1)
  | otherwise = error "oops"

gravity :: Moon -> [Moon] -> Moon
gravity moon [] = moon
gravity moon (m:ms) =
  let (pos, vel) = moon
      ((x, y, z), (dx, dy, dz)) = (pos, vel)
      ((x', y', z'), _) = m
      newVel = (dx + gravity' x x', dy + gravity' y y', dz + gravity' z z')
  in gravity (pos, newVel) ms

step :: [Moon] -> [Moon]
step moons = updatePositions $ updateVelocity moons
  where updateVelocity []        = []
        updateVelocity (moon:xs) = gravity moon moons : updateVelocity xs
        updatePositions []       = []
        updatePositions (((x, y, z), v):xs) =
          let (dx, dy, dz) = v
          in ((x + dx, y + dy, z + dz), v) : updatePositions xs

energy :: [Moon] -> Integer
energy = foldl (\acc moon -> acc + potentialEnergy moon * kineticEnergy moon) 0
  where potentialEnergy ((x, y, z), _) = abs x + abs y + abs z
        kineticEnergy (_, (x, y, z)) = abs x + abs y + abs z

example = readMoons ["<x=-1, y=  0, z= 2>", "<x= 2, y=-10, z=-7>", "<x= 4, y= -8, z= 8>", "<x= 3, y=  5, z=-1>"]

solveP1 :: [Moon] -> Integer
solveP1 moons = energy $ (iterate step moons !! 1000)

-- part2, compute periods for each dimension, then find the common multiple for each period

type Dimension = ([Integer], [Integer])

packMoons :: [Moon] -> [Dimension]
packMoons moons = packMoons' 0 moons
  where thd3 (_, _, z) = z
        snd3 (_, y, _) = y
        fst3 (x, _, _) = x
        packMoons' 2 xs = packDimensions thd3 xs : []
        packMoons' 1 xs = packDimensions snd3 xs : packMoons' 2 xs
        packMoons' 0 xs = packDimensions fst3 xs : packMoons' 1 xs
        packDimensions f xs = foldr (\(p, v) (pos, vel) -> (f p : pos, f v : vel)) ([], []) xs

packStep :: Dimension -> Dimension
packStep (ps, vs) = (ps', vs')
  where gravities p ps = sum [gravity' p p' | p' <- ps]
        vs' = [v + gravities p ps | (p, v) <- zip ps vs]
        ps' = [p + v | (p, v) <- zip ps vs']

packRepeat :: Dimension -> Integer
packRepeat dim = packRepeat' 1 dim
  where packRepeat' count ds = let newDim = packStep ds
                               in if newDim == dim then count else packRepeat' (count + 1) newDim

solveP2 = common . map packRepeat . packMoons
  where common = foldl (\x y -> x * y `div` gcd x y) 1

main :: IO ()
main = interact $ show . solveP2 . readMoons . lines
