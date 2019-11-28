-- | https://adventofcode.com/2018/day/2
-- | Find text distance and return common letter

distance :: Int -> String -> String -> Int
distance d [] []         = d
distance d _ []          = d + 1
distance d [] _          = d + 1
distance d (a:as) (b:bs) = distance (d + d') as bs
                           where d' = if a == b then 0 else 1

getCommons :: [String] -> [String] -> (String, String)
getCommons [] []         = ("error", "")
getCommons [] _          = ("error", "")
getCommons (_:xs) []     = getCommons xs (tail xs)
getCommons (a:xs) (b:bs) = if distance 0 a b == 1 then (a, b) else getCommons (a:xs) bs

commonChar' :: String -> String -> String -> String
commonChar' acc [] []         = acc
commonChar' acc _ []          = acc
commonChar' acc [] _          = acc
commonChar' acc (a:as) (b:bs) = commonChar' acc' as bs
                                where acc' = if a == b then a : acc else acc

commonChar :: (String, String) -> String
commonChar (a, b) = reverse $ commonChar' [] a b

solve :: [String] -> String
solve xs = commonChar $ getCommons xs (tail xs)

main :: IO ()
main = interact $ show . solve . lines
