import gleam/io
import gleam/string
import gleam/result
import gleam/list
import gleam/int

// My Prelude
pub external fn cwd() -> a =
  "file" "get_cwd"

pub external fn read_file(String) -> Result(Bytes, String) =
  "file" "read_file"

pub external fn byte_to_string(Bytes) -> String =
  "erlang" "binary_to_list"

pub external type Bytes

pub fn read_file_string(filepath) {
  result.map(over: read_file(filepath), with: byte_to_string)
}

pub fn read_lines(filepath) {
  result.map(
    over: read_file_string(filepath),
    with: fn(s) { string.split(s, on: "\n") },
  )
}

pub fn debug(term, txt) {
  io.print(txt)
  io.print(": ")
  io.debug(term)
}

pub fn void(_term) {
  ""
}

pub fn solve(filepath) {
  // debug("cwd", cwd())
  case read_lines(filepath) {
    Ok(res) -> void(debug(do_solve2(res), "Solution"))
    Error(err) -> void(debug(err, "read error"))
  }
}

// Part 1
pub type Acc {
  Acc(prev: Int, count: Int)
}

pub fn do_solve(inputs: List(String)) {
  inputs
  |> list.filter_map(with: int.parse)
  |> list.fold(
    Acc(99999, 0),
    fn(acc, n) {
      let Acc(prev, count) = acc
      case n > prev {
        True -> Acc(n, count + 1)
        False -> Acc(n, count)
      }
    },
  )
}

// Part 2
pub type Accs {
  Accs(prev: List(Int), count: Int)
}

pub fn is_gt(a, b) {
  a > b
}

pub fn do_solve2(inputs: List(String)) {
  inputs
  |> list.filter_map(with: int.parse)
  |> list.window(3)
  |> list.fold(
    Accs([999, 999, 999], 0),
    fn(acc, n) {
      let Accs(prev, count) = acc
      case int.sum(n) > int.sum(prev) {
        True -> Accs(n, count + 1)
        False -> Accs(n, count)
      }
    },
  )
}
