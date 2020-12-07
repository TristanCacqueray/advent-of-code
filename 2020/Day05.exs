defmodule Day05 do
  def bin_to_int(xs, acc) do
    case xs do
      [] ->
        acc

      [head | tail] ->
        case head === "B" or head === "R" do
          true -> bin_to_int(tail, acc + (:math.pow(2, Enum.count(xs) - 1) |> round))
          false -> bin_to_int(tail, acc)
        end
    end
  end

  def bin_to_int(xs) do
    bin_to_int(xs, 0)
  end

  def parse(s) do
    s |> String.graphemes() |> Enum.split(7)
  end

  def id(seat) do
    {row, col} = seat
    (row |> bin_to_int) * 8 + (col |> bin_to_int)
  end

  def findSeat(xs) do
    Range.new(0, 930)
    |> Enum.find(0, fn x ->
      not Enum.member?(xs, x) and Enum.member?(xs, x - 1) and Enum.member?(xs, x + 1)
    end)
  end
end

examples = [
  "BFFFBBFRRR",
  "FFFBBBFRRR",
  "BBFFBBFRLL"
]

IO.inspect(
  #  examples
  IO.read(:stdio, :all)
  |> String.split("\n")
  |> Enum.map(fn s ->
    s
    |> Day05.parse()
    |> Day05.id()
  end)
  |> Day05.findSeat()
)
