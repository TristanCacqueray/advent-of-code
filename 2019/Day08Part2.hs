{-# LANGUAGE OverloadedStrings #-}

-- | Print Space Image Format

module Day08Part2 where

import           Day08Part1 hiding (main)

decode :: Image -> Layer
decode ((x, y), layers) = decode' [2 | _ <- [1..x], _ <- [1..y]] layers
  where decode' :: Layer -> [Layer] -> Layer
        decode' result []         = result
        decode' result (layer:xs) = decode' (stackLayer result layer []) xs
        stackLayer :: Layer -> Layer -> Layer -> Layer
        stackLayer [] [] output = reverse output
        stackLayer (a:as) (b:bs) result = let color = if a == 2 then b else a
                                          in stackLayer as bs (color:result)

showLayer :: Dimension -> Layer -> String
showLayer _ [] = ""
showLayer dim list = let (x, _) = dim
                         c = fromInteger x
                     in show (take c list) ++ "\n" ++ showLayer dim (drop c list)

dimension :: Dimension
dimension = (25, 6)

main :: IO ()
main = interact $ showLayer dimension . decode . readImage dimension . convertInput
