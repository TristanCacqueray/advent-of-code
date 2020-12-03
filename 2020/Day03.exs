# how many trees would you encounter?

example = [
  "..##.......",
  "#...#...#..",
  ".#....#..#.",
  "..#.#...#.#",
  ".#...##..#.",
  "..#.##.....",
  ".#.#.#....#",
  ".#........#",
  "#.##...#...",
  "#...##....#",
  ".#..#...#.#"
]

defmodule Day03 do
  def solve(xs, _x, y, _dx, _dy) when y >= length(xs) do
    0
  end

  def solve(xs, x, y, dx, dy) do
    line = Enum.at(xs, y)

    tree =
      case Enum.at(line, rem(x, length(line))) do
        35 -> 1
        _ -> 0
      end

    tree + solve(xs, x + dx, y + dy, dx, dy)
  end

  def solveAll(xs) do
    [
      solve(xs, 0, 0, 1, 1),
      solve(xs, 0, 0, 3, 1),
      solve(xs, 0, 0, 5, 1),
      solve(xs, 0, 0, 7, 1),
      solve(xs, 0, 0, 1, 2)
    ]
  end
end

# IO.puts(Day03.solve(example |> Enum.map(fn x -> x |> String.to_charlist() end), 0, 0))
IO.puts(
  IO.read(:stdio, :all)
  |> String.split("\n", trim: true)
  |> Enum.map(fn x -> x |> String.to_charlist() end)
  |> Day03.solveAll()
  |> Enum.reduce(&*/2)
)
