// test solution: 198
// part1 solution: 845186
// part2 solution: 4636702
import day01.{debug, read_lines, void}
import gleam/list
import gleam/int
import gleam/float
import gleam/string
import gleam/result

pub external fn pow_float(Int, Int) -> Float =
  "math" "pow"

pub fn pow(x, y) -> Int {
  pow_float(x, y)
  |> float.round
}

pub fn solve_main(filepath, cb) {
  case read_lines(filepath) {
    Ok(res) -> void(debug(cb(res), "Solution"))
    Error(err) -> void(debug(err, "read error"))
  }
}

pub fn solve_p1(filepath) {
  solve_main(filepath, do_solve)
}

pub type Bit {
  Bit(low_count: Int, high_count: Int)
}

pub fn new_bits(size) {
  list.repeat(Bit(0, 0), size)
}

pub fn get_most_common(bit) {
  let Bit(low, high) = bit
  case low > high {
    True -> 0
    False -> 1
  }
}

pub fn get_least_common(bit) {
  let Bit(low, high) = bit
  case low <= high {
    True -> 0
    False -> 1
  }
}

pub fn update_bits(bits, line) {
  case string.length(line) == list.length(bits) {
    True ->
      string.to_graphemes(line)
      |> list.zip(bits)
      |> list.map(fn(tup: #(String, Bit)) {
        let Bit(low_count, high_count) = tup.1
        case tup.0 {
          "0" -> Bit(low_count + 1, high_count)
          _1 -> Bit(low_count, high_count + 1)
        }
      })
    False -> {
      case line {
        "" -> void("")
        _ -> debug(line, "Invalid line")
      }
      bits
    }
  }
}

fn go_bin_to_digit(acc, xs) {
  case xs {
    [head, ..rest] -> {
      let length = list.length(xs)
      go_bin_to_digit(acc + head * pow(2, length - 1), rest)
    }
    _ -> acc
  }
}

pub fn bin_to_digit(xs) {
  go_bin_to_digit(0, xs)
}

pub fn do_solve(inputs: List(String)) {
  let size = case list.first(inputs) {
    Ok(l) -> string.length(l)
  }
  let result = list.fold(inputs, new_bits(size), update_bits)
  case [list.map(result, get_most_common), list.map(result, get_least_common)]
  |> list.map(bin_to_digit) {
    [gamma, epsilon] -> gamma * epsilon
  }
}

// Part 2
pub fn binstring_to_digit(s) {
  s
  |> string.to_graphemes
  |> list.map(int.parse)
  |> result.all
  |> result.map(bin_to_digit)
}

pub fn solve(filepath) {
  solve_main(filepath, do_solve_p2)
}

fn do_solve_p2(inputs: List(String)) {
  let size = case list.first(inputs) {
    Ok(l) -> string.length(l)
  }
  case result.all([
    rec_solve_p2(size, inputs, 0, get_most_common),
    rec_solve_p2(size, inputs, 0, get_least_common),
  ]) {
    Ok([oxygen, co2]) -> oxygen * co2
  }
}

fn rec_solve_p2(
  size: Int,
  inputs: List(String),
  bit_pos: Int,
  cb: fn(Bit) -> Int,
) {
  case inputs {
    [result] ->
      result
      |> binstring_to_digit
    _otherwise -> {
      let result = list.fold(inputs, new_bits(size), update_bits)
      let bit = case list.at(result, bit_pos) {
        Ok(bit) ->
          cb(bit)
          |> int.to_string
      }
      let filtered =
        inputs
        |> list.filter(fn(s) {
          string.slice(from: s, at_index: bit_pos, length: 1) == bit
        })
      rec_solve_p2(size, filtered, bit_pos + 1, cb)
    }
  }
}
