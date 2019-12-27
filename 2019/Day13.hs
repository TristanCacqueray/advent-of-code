{-# LANGUAGE OverloadedStrings #-}
-- | Care Package

import qualified Data.Set as Set
import           Day09    hiding (main, solveP1, solveP2)

data TileID = Empty | Wall | Block | HPaddle | Ball
  deriving (Eq, Show)

type Tiles = (Int, Int, TileID)

tileID :: Int -> TileID
tileID 0 = Empty
tileID 1 = Wall
tileID 2 = Block
tileID 3 = HPaddle
tileID 4 = Ball
tileID _ = error "Unknown tileID"
showTile :: TileID -> Char
showTile Empty   = '.'
showTile Wall    = '#'
showTile Block   = 'X'
showTile HPaddle = '_'
showTile Ball    = 'o'

evaluateUntilIO :: IntcodeComputer -> IntcodeComputer
evaluateUntilIO computer = let next = evaluate computer
                           in case state next of
                                Halt      -> next
                                NeedInput -> next
                                Output    -> next
                                _         -> evaluateUntilIO next

draw :: Int -> IntcodeComputer -> (IntcodeComputer, Int, [Tiles])
draw s c = draw' (c, s, [])
  where o computer = fromInteger $ head (outputs computer)
        draw' :: (IntcodeComputer, Int, [Tiles]) -> (IntcodeComputer, Int, [Tiles])
        draw' (computer, score, tiles) =
          let cx = evaluateUntilIO computer
              cy = evaluateUntilIO cx
              ct = evaluateUntilIO cy
              tile = (o cx, o cy, tileID (o ct))
              next :: (IntcodeComputer, Int, [Tiles])
              next = if (o cx == (-1)) && (o cy == 0)
                     then (ct, o ct, tiles)
                     else (ct, score, tile : tiles)
          in if state cx == Halt || state cx == NeedInput
             then next
             else draw' next

solveP1 :: IntcodeComputer -> Int
solveP1 = length . filter (\(_, _, t) -> t == Block) . (\(_, _, t) -> t) . draw 0

-- main = interact $ show . solveP1 . createComputer

render :: (Int, [Tiles]) -> String
render = unlines . reverse . render'
  where render' :: (Int, [Tiles]) -> [String]
        render' (score, tiles) = renderTiles tiles : ["", "score: " ++ show score]
        getTile _ [] = ' '
        getTile (x, y) ((tx, ty, ti):ts)
          | x == tx && y == ty = showTile ti
          | otherwise = getTile (x, y) ts
        renderTiles' :: (Int, Int) -> (Int, Int) -> [Tiles] -> String
        renderTiles' (x, maxX) (y, maxY) tiles
          | y > maxY = []
          | x > maxX = '\n' : renderTiles' (0, maxX) (y + 1, maxY) tiles
          | otherwise = getTile (x, y) tiles : renderTiles' (x + 1, maxX) (y, maxY) tiles
        renderTiles :: [Tiles] -> String
        renderTiles tiles =
          let xs = map (\(x, _, _) -> x) tiles
              ys = map (\(_, y, _) -> y) tiles
          in  renderTiles' (0, maximum xs) (0, maximum ys) tiles

play :: [String] -> [String]
play (computerString:input) = play' 0 input [] initComputer
  where initComputer = addInput 0 $ createComputer (addCoin computerString)
        addCoin s = "2" ++ (drop 1 s)
        i "a" = (-1)
        i "d" = 1
        i _   = 0
        play' :: Int -> [String] -> [Tiles] -> IntcodeComputer -> [String]
        play' s (x:xs) tiles computer =
          let (next, score, nextTiles) = draw s $ addInput (i x) computer
              newTiles = nextTiles ++ tiles
          in "Enter to play:\n" : render (score, newTiles) :
             play' score xs newTiles next

autoPlay :: [String] -> [String]
autoPlay (computerString:_) = play' 0 0 (initComputer, [])
  where initComputer = addInput 0 $ createComputer (addCoin computerString)
        addCoin s = "2" ++ drop 1 s
        getTile _ [] = (20, 20)
        getTile tid ((x, y, tid'):xs)
          | tid == tid' = (x, y)
          | otherwise   = getTile tid xs

        -- Naive direction that does not care about ball velocity
        moveToward :: Int -> Int -> Int
        moveToward paddlex ballx =
          case compare ballx paddlex of
            GT ->  1
            EQ ->  0
            LT -> -1

        -- Remove tiles that have been cleared
        removeEmpties :: [Tiles] -> [Tiles]
        removeEmpties = reverse . removeEmpties' Set.empty
          where removeEmpties' _ [] = []
                removeEmpties' s (t:ts) =
                  let (x, y, tID) = t
                      coord = (x, y)
                      newS = if tID == Empty then Set.insert coord s else s
                  in if Set.member coord s
                     then removeEmpties' newS ts
                     else t : removeEmpties' newS ts

        -- Remove extra (4,4,Ball) from draw output
        combineTiles :: [Tiles] -> [Tiles] -> [Tiles]
        combineTiles tiles allTiles =
          let tiles' = drop 1 tiles
          in removeEmpties $ tiles' ++ allTiles

        play' :: Int -> Int -> (IntcodeComputer, [Tiles]) -> [String]
        play' step prevScore prevState =
          let (prevComputer, prevTiles) = prevState
              -- Compute naive input
              (ballX, _) = getTile Ball prevTiles
              (paddleX, _) = getTile HPaddle prevTiles
              dir = moveToward paddleX ballX
              (newComputer, newScore, tiles) = draw prevScore $ addInput (toInteger dir) prevComputer
              curTiles = combineTiles  tiles prevTiles

              -- Fix input when result is incorrect, only used after below `if`
              (nballX, _) = getTile Ball curTiles
              (npaddleX, _) = getTile HPaddle curTiles
              fixedDir = if nballX > npaddleX then (1::Integer) else (-1)
              (fixComputer, fixScore, fixTiles) = draw prevScore $ addInput (toInteger fixedDir) prevComputer
              curFixes = combineTiles fixTiles prevTiles

              debug = ("step:" ++ show step ++ " ball:" ++ show ballX)

          in if abs (nballX - npaddleX) < 2
             then debug : render (newScore, curTiles) :
                  play' (step + 1) newScore (newComputer, curTiles)
             else debug : render (fixScore, curFixes) :
                  play' (step + 1) fixScore (fixComputer, curFixes)


main = interact $ unlines . autoPlay . lines
