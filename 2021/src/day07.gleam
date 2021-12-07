// part1 test solution: 37
// part1 solution: 336040
// part2 test solution: 168
// part2 solution: 94813675
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

fn do_solve(inputs, adapt) {
  inputs
  |> string.join("")
  |> string.split(",")
  |> list.filter_map(with: int.parse)
  |> fn(xs) {
    try mi = list.reduce(xs, min)
    try ma = list.reduce(xs, max)
    // For each possible postion
    list.range(mi, ma + 1)
    |> list.map(fn(all) {
      // Get the total distance for each crab
      xs
      |> list.map(fn(x) { adapt(absolute_value(x - all)) })
      |> int.sum
    })
    |> list.reduce(min)
    |> Ok
  }
}

pub fn solve_p1(filepath) {
  solve_main(filepath, do_solve(_, function.identity))
}

pub fn solve(filepath) {
  solve_main(filepath, do_solve(_, fn(x) { x * { x + 1 } / 2 }))
}
