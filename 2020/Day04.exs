defmodule Day04 do
  def parse(s) do
    s |> String.split() |> Enum.map(fn e -> e |> String.split(":") |> List.to_tuple() end)
  end

  def getFields(l) do
    l |> Enum.map(fn e -> e |> elem(0) end) |> MapSet.new()
  end

  def isValidFields(m) do
    MapSet.new([
      "byr",
      "iyr",
      "eyr",
      "hgt",
      "hcl",
      "ecl",
      "pid"
      #        "cid"
    ])
    |> MapSet.subset?(m)
  end

  def getItems(l) do
    l |> Map.new()
  end

  def isInRange(s, l, h) do
    case s |> String.to_integer() do
      n when n >= l and n <= h -> true
      _ -> false
    end
  end

  def isValidByr(s) do
    isInRange(s, 1920, 2002)
  end

  def isValidIyr(s) do
    isInRange(s, 2010, 2020)
  end

  def isValidEyr(s) do
    isInRange(s, 2020, 2030)
  end

  def isValidHgt(s) do
    case s |> String.ends_with?("cm") do
      true -> isInRange(s |> String.trim_trailing("cm"), 150, 193)
      false -> isInRange(s |> String.trim_trailing("in"), 59, 76)
    end
  end

  defmodule Parser do
    import NimbleParsec

    number = ascii_char([?0..?9])
    hexa = choice([number, ascii_char([?a..?f])])

    defparsec(:hcl, string("#") |> concat(duplicate(hexa, 6)))
    defparsec(:pid, duplicate(number, 9))
  end

  def isValidHcl(s) do
    case s |> Parser.hcl() do
      {:ok, _, "", _, _, _} -> true
      _ -> false
    end
  end

  def isValidEcl(s) do
    ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] |> Enum.member?(s)
  end

  def isValidPid(s) do
    case s |> Parser.pid() do
      {:ok, _, "", _, _, _} -> true
      _ -> false
    end
  end

  def isValidItems(m) do
    m |> Map.keys() |> MapSet.new() |> isValidFields and
      m |> Map.get("byr", "0") |> isValidByr and
      m |> Map.get("iyr", "0") |> isValidIyr and
      m |> Map.get("eyr", "0") |> isValidEyr and
      m |> Map.get("hgt", "0") |> isValidHgt and
      m |> Map.get("hcl", "0") |> isValidHcl and
      m |> Map.get("ecl", "0") |> isValidEcl and
      m |> Map.get("pid", "0") |> isValidPid
  end

  def solve(l) do
    l
    |> Enum.reduce(0, fn e, acc ->
      case e == true do
        true -> 1 + acc
        false -> acc
      end
    end)
  end
end

IO.inspect(
  IO.read(:stdio, :all)
  |> String.split("\n\n")
  |> Enum.map(fn s ->
    s
    |> Day04.parse()
    |> Day04.getItems()
    |> Day04.isValidItems()
  end)
  |> Day04.solve()
)
