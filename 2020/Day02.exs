# How many passwords are valid?

example = [
  "1-3 a: abcde",
  "1-3 b: cdefg",
  "2-9 c: ccccccccc"
]

defmodule Day02 do
  def test({rangeStart, rangeEnd, char, s}) do
    case s
         |> String.to_charlist()
         |> List.foldl(
           0,
           fn x, acc ->
             case x == char do
               true -> acc + 1
               false -> acc
             end
           end
         ) do
      n when n >= rangeStart and n <= rangeEnd -> true
      _ -> false
    end
  end

  def getC(s, p) do
    s |> String.at(p - 1) |> String.to_charlist() |> hd
  end

  def test2({rangeStart, rangeEnd, char, s}) do
    case {s |> getC(rangeStart), s |> getC(rangeEnd)} do
      {p1, p2} when (p1 == char and p2 != char) or (p1 != char and p2 == char) -> true
      _ -> false
    end
  end

  def parse(s) do
    [range, char, value] = s |> String.split()
    [rangeStart, rangeEnd] = range |> String.split("-")
    c = char |> String.split(":") |> hd

    {rangeStart |> String.to_integer(), rangeEnd |> String.to_integer(),
     String.to_charlist(c) |> hd, value}
  end

  def solve(xs) do
    xs
    |> Enum.map(&Day02.parse/1)
    |> Enum.map(&Day02.test2/1)
    |> List.foldl(0, fn x, acc ->
      case x do
        true -> acc + 1
        false -> acc
      end
    end)
  end
end

# IO.puts(Day02.test2({1, 3, "a" |> String.to_charlist() |> hd, "abcde"}))
# IO.puts(Enum.join(Tuple.to_list(Day02.parse(example |> hd))))
# IO.puts(Day02.solve(example))

IO.puts(
  IO.read(:stdio, :all)
  |> String.split("\n", trim: true)
  |> Day02.solve()
)
