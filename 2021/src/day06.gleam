// part1 test solution: 5934
// part1 solution: 353079
// part2 test solution: 26984457539
// part2 solution: 1605400130036
import day01.{debug, read_lines, void}
import day03.{solve_main}
import gleam/list
import gleam/int
import gleam/float
import gleam/string
import gleam/result
import gleam/option.{None, Option, Some}
import gleam/bool
import gleam/function
import gleam/order.{Eq, Gt, Lt}
import gleam/set.{Set}
import gleam/iterator.{Done, Next}
import gleam/map.{Map}
import gleam/pair

pub type Age =
  Int

pub type Fishs =
  Map(Age, Int)

// Helpers to manipulate a fish group
pub type FishGroup =
  #(Age, Int)

fn get_age(fg: FishGroup) {
  pair.first(fg)
}

fn set_age(fg: FishGroup, age: Int) {
  pair.map_first(fg, constant(age))
}

fn get_count(fg: FishGroup) {
  pair.second(fg)
}

// Eval a single day
fn eval_fish(fg: FishGroup) {
  case get_age(fg) {
    0 -> [set_age(fg, 6), set_age(fg, 8)]
    age -> [set_age(fg, age - 1)]
  }
}

fn eval(fishs: Fishs, day: Int) {
  case day == 0 {
    True -> fishs
    False ->
      fishs
      |> map.to_list
      |> list.flat_map(eval_fish)
      |> list.fold(
        map.new(),
        fn(acc, fg) {
          map.update(acc, get_age(fg), increment(_, get_count(fg)))
        },
      )
      |> eval(day - 1)
  }
}

fn increment(prev: Option(Int), v: Int) {
  case prev {
    None -> v
    Some(count) -> count + v
  }
}

fn do_solve(inputs, day) {
  inputs
  |> string.join("")
  |> string.split(",")
  |> list.filter_map(with: int.parse)
  |> list.fold(map.new(), fn(acc, v) { map.update(acc, v, increment(_, 1)) })
  |> eval(day)
  |> map.values
  |> int.sum
}

pub fn solve(filepath, day) {
  solve_main(filepath, do_solve(_, day))
}

// Prelude addition: function.constant
pub fn constant(value: a) -> fn(b) -> a {
  fn(_) { value }
}
