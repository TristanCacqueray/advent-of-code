-- Check with:
-- $ elm repl
-- > import Day01
-- > Day01.part1

module Day01 exposing (..)

import String

solve n (count, prevM) =
  let inc = case prevM of
        Nothing -> 0
        Just prev ->
          if n > prev then
            1
          else
            0
   in (count + inc, Just n)

part1 =
  input
  |> String.words
  |> List.filterMap String.toInt
  |> List.foldl solve (0, Nothing)
  |> Tuple.first

input = """
124
125
...
"""
