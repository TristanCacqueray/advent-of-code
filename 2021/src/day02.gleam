import day01.{debug, read_lines, void}
import gleam/list
import gleam/int
import gleam/string
import gleam/result

pub fn solve(filepath) {
  case read_lines(filepath) {
    Ok(res) -> void(debug(do_solve2(res), "Solution"))
    Error(err) -> void(debug(err, "read error"))
  }
}

pub type Command {
  Forward(units: Int)
  Down(units: Int)
  Up(units: Int)
}

pub fn parse_command(line: String) {
  let parse = fn(x, c) { result.map(over: int.parse(x), with: c) }
  case string.split(line, on: " ") {
    ["forward", x] -> parse(x, Forward)
    ["down", x] -> parse(x, Down)
    ["up", x] -> parse(x, Up)
    _ -> Error(Nil)
  }
}

// Part 1
pub type Ship {
  Ship(position: Int, depth: Int)
}

pub fn do_solve(inputs: List(String)) {
  inputs
  |> list.filter_map(with: parse_command)
  |> list.fold(
    Ship(0, 0),
    fn(ship, command) {
      let Ship(position, depth) = ship
      case command {
        Forward(unit) -> Ship(position + unit, depth)
        Down(unit) -> Ship(position, depth + unit)
        Up(unit) -> Ship(position, depth - unit)
      }
    },
  )
  |> fn(ship) {
    let Ship(position, depth) = ship
    position * depth
  }
}

// Part 2
pub type ShipAim {
  ShipAim(position: Int, depth: Int, aim: Int)
}

pub fn do_solve2(inputs: List(String)) {
  inputs
  |> list.filter_map(with: parse_command)
  |> list.fold(
    ShipAim(0, 0, 0),
    fn(ship, command) {
      let ShipAim(position, depth, aim) = ship
      let set_aim = fn(aim) { ShipAim(position, depth, aim) }
      case command {
        Forward(unit) -> ShipAim(position + unit, depth + aim * unit, aim)
        Down(unit) -> set_aim(aim + unit)
        Up(unit) -> set_aim(aim - unit)
      }
    },
  )
  |> fn(ship) {
    let ShipAim(position, depth, _aim) = ship
    position * depth
  }
}
