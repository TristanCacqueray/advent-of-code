-- | advent of code 2017-2

lineChecksum :: String -> Integer
lineChecksum xs =
  let vals = map read $ words xs
  in maximum vals - minimum vals

main = interact $ show . sum . map lineChecksum . lines
