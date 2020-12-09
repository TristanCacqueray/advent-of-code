example = [
  "35",
  "20",
  "15",
  "25",
  "47",
  "40",
  "62",
  "55",
  "65",
  "95",
  "102",
  "117",
  "150",
  "182",
  "127",
  "219",
  "299",
  "277",
  "309",
  "576"
]

defmodule XMAS do
  def isValid(xs, num) do
    xs
    |> Enum.with_index()
    |> Enum.find(fn {x1, pos} ->
      xs
      |> Enum.drop(pos + 1)
      |> Enum.find(fn x2 ->
        x1 + x2 == num
      end)
    end) != nil
  end

  def findInvalid(xs, sz, pos) do
    num = xs |> Enum.at(pos)

    case xs |> Enum.drop(pos - sz) |> Enum.take(sz) |> isValid(num) do
      false -> num
      true -> (num && findInvalid(xs, sz, pos + 1)) || -1
    end
  end

  def findInvalid(xs, sz) do
    xs |> findInvalid(sz, sz)
  end

  def findContiguous(xs, num, history, acc, remaining) do
    case xs do
      [] ->
        -1

      [x | rest] ->
        case x + acc do
          n when n == num -> [x | history]
          n when n < num -> findContiguous(rest, num, [x | history], n, remaining)
          _ -> remaining |> Enum.drop(1) |> findContiguous(num)
        end
    end
  end

  def findContiguous(xs, num) do
    findContiguous(xs, num, [], 0, xs)
  end

  def crack(xs, sz) do
    range = xs |> findContiguous(findInvalid(xs, sz))
    Enum.min(range) + Enum.max(range)
  end
end

IO.inspect(
  # example
  IO.read(:stdio, :all)
  |> String.split("\n", trim: true)
  |> Enum.map(fn x -> String.to_integer(x) end)
  |> XMAS.crack(25)
)
