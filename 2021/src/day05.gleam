// part1 test solution: 5
// part1 solution: 4993
// part2 test solution: 12
// part2 solution: 21101
import day01.{debug, read_lines, void}
import day03.{solve_main}
import gleam/list
import gleam/int.{absolute_value, max, min}
import gleam/float
import gleam/string
import gleam/result
import gleam/option.{None, Some}
import gleam/bool
import gleam/function
import gleam/order.{Eq, Gt, Lt}
import gleam/set.{Set}
import gleam/iterator.{Done, Next}

pub type Point {
  Point(x: Int, y: Int)
}

pub type Line {
  Line(p1: Point, p2: Point)
}

pub fn range(x, y) {
  let end = case x < y {
    True -> y + 1
    False -> y - 1
  }
  list.range(x, end)
}

pub fn line_to_points(line: Line) -> List(Point) {
  let Line(Point(x1, y1), Point(x2, y2)) = line
  case x1 == x2, y1 == y2 {
    True, _ ->
      range(y1, y2)
      |> list.map(Point(x1, _))
    _, True ->
      range(x1, x2)
      |> list.map(Point(_, y1))
    // uncomment for p1
    // _, _ -> []
    _, _ ->
      list.zip(range(x1, x2), range(y1, y2))
      |> list.map(fn(p: #(Int, Int)) { Point(p.0, p.1) })
  }
}

// Consume the iterator and collect duplicated points
fn get_duplicate(xs, acc, result) -> Set(Point) {
  case iterator.step(xs) {
    Done -> result
    Next(x, xs) ->
      case set.contains(acc, x) {
        True -> get_duplicate(xs, acc, set.insert(result, x))
        False -> get_duplicate(xs, set.insert(acc, x), result)
      }
  }
}

fn do_solve(inputs) {
  inputs
  |> list.filter(function.compose(string.is_empty, bool.negate))
  |> list.map(parse_line)
  |> result.all
  |> result.map(fn(lines) {
    lines
    |> iterator.from_list
    |> iterator.map(function.compose(line_to_points, iterator.from_list))
    |> iterator.flatten
    |> get_duplicate(set.new(), set.new())
    |> set.size
  })
}

pub fn solve(filepath) {
  solve_main(filepath, do_solve)
}

fn parse_point(str: String) -> Result(Point, Nil) {
  let [x, y] = string.split(str, ",")
  try x = int.parse(x)
  try y = int.parse(y)
  Ok(Point(x, y))
}

fn parse_line(str: String) -> Result(Line, Nil) {
  let [p1, p2] = string.split(str, " -> ")
  try p1 = parse_point(p1)
  try p2 = parse_point(p2)
  Ok(Line(p1, p2))
}
