# find the two entries that sum to 2020, and then multiply those two numbers together.

example = [
  1721,
  979,
  366,
  299,
  675,
  1456
]

defmodule Day01 do
  def test(a, b) do
    case a + b == 2020 do
      true ->
        a * b

      false ->
        nil
    end
  end

  def solve2(x, xs) do
    xs |> Enum.map(fn y -> test(x, y) end)
  end

  def test(a, b, c) do
    case a + b + c == 2020 do
      true ->
        a * b * c

      false ->
        nil
    end
  end

  def solve3(x, xs) do
    xs |> Enum.map(fn y -> xs |> Enum.map(fn z -> test(x, y, z) end) end)
  end

  def solve(xs, test) do
    [head | _tail] =
      xs
      |> Enum.map(fn x -> test.(x, xs) end)
      |> List.flatten()
      |> Enum.filter(&(!is_nil(&1)))

    head
  end
end

# IO.puts(Solve.part1(example))

IO.puts(
  IO.read(:stdio, :all)
  |> String.split("\n", trim: true)
  |> Enum.map(fn x -> String.to_integer(x) end)
  |> Day01.solve(&Day01.solve3/2)
)
