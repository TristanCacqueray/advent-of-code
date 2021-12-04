// test solution: 4512
// part1 solution: 21607
// part2 solution: 19012
import day01.{debug, read_lines, void}
import day03.{solve_main}
import gleam/list
import gleam/int
import gleam/float
import gleam/string
import gleam/result
import gleam/option.{None, Some}
import gleam/bool
import gleam/function
import gleam/set.{Set}

// More prelude
pub fn words(x: String) -> List(String) {
  x
  |> string.split(" ")
  |> list.filter(function.compose(string.is_empty, bool.negate))
}

pub fn head(xs: List(a)) -> a {
  case list.first(xs) {
    Ok(x) -> x
  }
}

pub fn cons(x: a, xs: List(a)) -> List(a) {
  [x, ..xs]
}

type Board {
  Board(id: Int, rows: List(List(Int)), columns: List(List(Int)))
}

fn make_board(id, xs: List(List(Int))) {
  Board(
    id,
    xs,
    xs
    |> list.transpose,
  )
}

fn is_winner(numbers, board) {
  let Board(_, rows, columns) = board
  list.flatten([rows, columns])
  |> list.any(fn(xs) {
    xs
    |> list.all(fn(x) { set.contains(numbers, x) })
  })
}

// Part 1
fn parse_board(
  inputs: List(String),
  id: Int,
  acc: List(List(Int)),
) -> #(List(String), Board) {
  case inputs {
    [] as rest | ["", ..rest] -> #(rest, make_board(id, acc))
    [x, ..rest] ->
      parse_board(
        rest,
        id,
        x
        |> words
        |> list.filter_map(int.parse)
        |> cons(acc),
      )
  }
}

fn parse_boards(inputs: List(String), id: Int, acc: List(Board)) {
  case inputs {
    [] -> acc
    _ -> {
      let result = parse_board(inputs, id, [])
      parse_boards(
        result.0,
        id + 1,
        result.1
        |> cons(acc),
      )
    }
  }
}

type Game {
  Game(numbers: List(Int), boards: List(Board))
}

fn parse(inputs: List(String)) {
  let numbers =
    inputs
    |> head
    |> string.split(",")
    |> list.filter_map(int.parse)

  let boards =
    inputs
    |> list.drop(2)
    |> parse_boards(0, [])

  Game(numbers, boards)
}

fn find_winner(numbers, boards) {
  case boards {
    [] -> None
    [board, ..rest] ->
      case is_winner(numbers, board) {
        True -> Some(board)
        False -> find_winner(numbers, rest)
      }
  }
}

fn eval_game(game, pos) {
  let Game(numbers, boards) = game
  let current =
    numbers
    |> list.take(pos)
  case current
  |> set.from_list
  |> find_winner(boards) {
    Some(winner) -> #(current, winner)
    None -> eval_game(game, pos + 1)
  }
}

fn get_result(result: #(List(Int), Board)) {
  let numbers = result.0
  let Board(_, rows, _) = result.1
  let last = case list.last(numbers) {
    Ok(x) -> x
  }
  let numbers_set = set.from_list(numbers)
  let unmarked =
    rows
    |> list.flatten
    |> list.filter(fn(x) {
      numbers_set
      |> set.contains(x)
      |> bool.negate
    })
    |> int.sum
  unmarked * last
}

fn do_solve_p1(inputs) {
  parse(inputs)
  |> eval_game(0)
  |> get_result
}

pub fn solve_p1(filepath) {
  solve_main(filepath, do_solve_p1)
}

// Part 2
fn is_board(x, y) {
  let Board(xid, _, _) = x
  let Board(yid, _, _) = y
  xid == yid
}

fn eval_looser(game, pos) {
  let Game(numbers, boards) = game
  let current =
    numbers
    |> list.take(pos)
  case current
  |> set.from_list
  |> find_winner(boards) {
    Some(winner) ->
      case boards
      |> list.filter(fn(x) {
        x
        |> is_board(winner)
        |> bool.negate
      }) {
        // That was the last winner
        [] -> #(current, winner)
        new_boards ->
          Game(numbers, new_boards)
          |> eval_looser(pos)
      }
    None -> eval_looser(game, pos + 1)
  }
}

fn do_solve_p2(inputs) {
  parse(inputs)
  |> eval_looser(0)
  |> get_result
}

pub fn solve(filepath) {
  solve_main(filepath, do_solve_p2)
}
