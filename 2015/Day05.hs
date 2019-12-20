{-# LANGUAGE OverloadedStrings #-}
-- |

module Day05 where

countVowel :: String -> Integer
countVowel = countVowel' []
  where
    countVowel' :: [Char] -> [Char] -> Integer
    countVowel' vowels [] = toInteger $ length vowels
    countVowel' vowels (x:xs)
      | x `elem` ['a', 'e', 'i', 'o', 'u'] = countVowel' (x:vowels) xs
      | otherwise                          = countVowel' vowels xs

twices :: String -> Bool
twices (x:y:xs)
  | x == y  = True
  | otherwise = twices (y:xs)
twices _ = False

blacklist :: String -> Bool
blacklist (x:y:xs) =
  case x:y:[] of
      "ab" -> True
      "cd" -> True
      "pq" -> True
      "xy" -> True
      _    -> blacklist (y:xs)
blacklist _ = False

niceString :: String -> Bool
niceString s = threeVowels s && twices s && (not . blacklist) s
  where threeVowels s = countVowel s >= 3

solveP1 :: String -> Integer
solveP1 = toInteger . length . filter id . map niceString . lines

main = interact $ show . solveP1
