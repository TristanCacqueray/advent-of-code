{-# LANGUAGE OverloadedStrings #-}
-- | Space Stoichiometry

module Day14 where

import           Test.HUnit

import qualified Data.Set    as S
import qualified Data.Text   as T
import           Debug.Trace

type Chemical = (Float, String)
type Reaction = ([Chemical], Chemical)
-- type Recipe = [(Float, String)]

readReaction :: String -> Reaction
readReaction s =
  let l = T.splitOn " => " (T.pack s)
      (inputs, output) = (T.splitOn ", " (head l), last l)
      readReaction' :: T.Text -> Chemical
      readReaction' r = let [quantity, chemical] = T.splitOn " " r
                        in (read (T.unpack quantity), T.unpack chemical)
  in (map readReaction' inputs, readReaction' output)

getReaction :: String -> [Reaction] -> Reaction
getReaction s (r:rs) =
  let (_, (_, outputName)) = r
  in if s == outputName then r else getReaction s rs
getReaction s _ = error ("No reaction produce: " ++ s)

merge :: [Chemical] -> [Chemical]
merge = merge' S.empty
  where merge' _ [] = []
        merge' s ((count, chemical):xs)
          | S.member chemical s = merge' s xs
          | otherwise =
              let tot = foldl (\acc (count', chemical') -> if chemical' == chemical then acc + count' else acc) count xs
              in (tot, chemical) : merge' (S.insert chemical s) xs

reactCount :: Float -> Float -> Float
reactCount need produces = fromInteger $ ceiling $ need / produces

reactionNeed :: [Reaction] -> Chemical -> [Chemical]
reactionNeed r (count, name) =
  let (inputs, (outputCount, _)) = getReaction name r
      reactionCount = reactCount count outputCount
      needs' = map (\(c, chem) -> (c * reactionCount, chem)) inputs
  in needs'

reactionNeeds :: [Reaction] -> [Chemical] -> [Chemical]
reactionNeeds r = merge . concatMap (reactionNeed r)

oreIngredients :: [Reaction] -> Chemical -> [Chemical]
oreIngredients reactions need = oreCount' [need]
  where getNext :: [Chemical] -> [Chemical] -> (Maybe Chemical, [Chemical])
        getNext [] rest = (Nothing, rest)
        getNext (c:cs) rest =
          let (_, name) = c
              (inputs, _) = getReaction name reactions
              needOre = snd (head inputs) == "ORE"
              isNeeded :: [Chemical] -> Bool
              isNeeded [] = False
              isNeeded ((_, "ORE"):_) = False
              isNeeded ((_, name'):cs') =
                let (inputs', _) = getReaction name' reactions
                in foldl (\acc (_, needName) ->
                            acc ||
                            needName == name ||
                            isNeeded [(0.0, needName)])
                   False inputs' || isNeeded cs'
          in if not needOre && not (isNeeded cs)
             then (Just c, cs ++ rest)
             else getNext cs (c:rest)

        oreCount' needs =
          let (next, rest) = getNext (merge needs) []
          in -- trace ("oreCount' (" ++ show needs ++ ") , next:" ++ show next ++ ", rest:" ++ show rest) $
             case next of
              Just chem -> oreCount' $ reactionNeed reactions chem ++ rest
              Nothing   -> rest

solver :: Float -> [Reaction] -> Integer
solver c r = round $ fst $ head $ reactionNeeds r $ oreIngredients r (c, "FUEL")

solveP1 :: [Reaction] -> Integer
solveP1 = solver 1

solveP2 :: [Reaction] -> Integer
solveP2 reactions = round $ solveP2' 1.0 1.0 LT (1000000000000 `div` 2)
  where solveP2' :: Float -> Float -> Ordering -> Integer -> Float
        solveP2' count prev prevCmp step =
          let oreNeeded = solver count reactions
              nextStep = max 1 $ step `div` 2
              cmp = compare oreNeeded 1000000000000
          in case cmp of
               GT -> if abs (count - prev) == 1 && prevCmp == LT
                     then prev else solveP2' (count - fromInteger step) count cmp nextStep
               EQ -> count
               LT -> solveP2' (count + fromInteger step) count cmp nextStep


main :: IO ()
main = interact $ show . solveP2 . map readReaction . lines


testP2 :: Test
testP2 = TestList $ map (\(count, reactions) ->
                            let r = map readReaction reactions
                            in TestCase $ assertEqual (show count) count (solveP2 r))
          examples2

testP1 :: Test
testP1 = TestList $ map (\(count, reactions) ->
                            let r = map readReaction reactions
                            in TestCase $ assertEqual (show count) count (solveP1 r))
          examples

examples2 = [
    (82892753, get 3)
  , (5586022, get 4)
  , (460664, get 5)
            ]
  where get c = snd (examples !! c)

examples = [
  (2, [
       "1 ORE => 1 G",
       "1 G => 1 A",
       "1 A => 1 B",
       "1 A => 1 C",
       "1 B, 1 C => 1 FUEL"]),
  (31, [
      "10 ORE => 10 A"
      , "1 ORE => 1 B"
      , "7 A, 1 B => 1 C"
      , "7 A, 1 C => 1 D"
      , "7 A, 1 D => 1 E"
      , "7 A, 1 E => 1 FUEL"
      ])
  , (165, [
          "9 ORE => 2 A",
          "8 ORE => 3 B",
          "7 ORE => 5 C",
          "3 A, 4 B => 1 AB",
          "5 B, 7 C => 1 BC",
          "4 C, 1 A => 1 CA",
          "2 AB, 3 BC, 4 CA => 1 FUEL"
        ])
  , (13312, [
          "157 ORE => 5 NZVS",
          "165 ORE => 6 DCFZ",
          "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL",
          "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ",
          "179 ORE => 7 PSHF",
          "177 ORE => 5 HKGWZ",
          "7 DCFZ, 7 PSHF => 2 XJWVT",
          "165 ORE => 2 GPVTF",
          "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
        ])
  , (180697, [
          "139 ORE => 4 NVRVD",
          "144 ORE => 7 JNWZP",
          "145 ORE => 6 MNCFX",
          "176 ORE => 6 VJHF",
          "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG",
          "17 NVRVD, 3 JNWZP => 8 VPVL",
          "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL",
          "22 VJHF, 37 MNCFX => 5 FWMGM",
          "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC",
          "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV",
          "1 NVRVD => 8 CXFTF",
          "1 VJHF, 6 MNCFX => 4 RFSQX"
          ])
  , (2210736, [
          "171 ORE => 8 CNZTR",
          "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL",
          "114 ORE => 4 BHXH",
          "14 VRPVC => 6 BMBT",
          "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT",
          "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW",
          "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW",
          "5 BMBT => 4 WPTQ",
          "189 ORE => 9 KTJDG",
          "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP",
          "12 VRPVC, 27 CNZTR => 2 XDBXC",
          "15 KTJDG, 12 BHXH => 5 XCVML",
          "3 BHXH, 2 VRPVC => 7 MZWV",
          "121 ORE => 7 VRPVC",
          "7 XCVML => 6 RJRHP",
          "5 BHXH, 4 VRPVC => 5 LTCX",
          "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
        ])
  ]
