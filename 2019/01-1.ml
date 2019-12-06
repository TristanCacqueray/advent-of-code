open List;;
open Utils;;

let requiredFuel x =
  (int_of_float (floor ((float x) /. 3.))) - 2;;

let rec sum = function
  | [] -> 0
  | h::t -> (requiredFuel h) + (sum t);;

let solveFile path =
  let lines = Utils.read_file path in
  let values = (List.map (function s -> int_of_string s) lines) in
  sum values;;

print_string (string_of_int (solveFile "01-1.input"))
