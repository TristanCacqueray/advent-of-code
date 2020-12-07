examples = [
  "light red bags contain 1 bright white bag, 2 muted yellow bags.",
  "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
  "bright white bags contain 1 shiny gold bag.",
  "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
  "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
  "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
  "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
  "faded blue bags contain no other bags.",
  "dotted black bags contain no other bags."
]

defmodule Parser do
  import NimbleParsec

  bagID = ascii_string([?a..?z], min: 1)

  bag = bagID |> ignore(string(" ")) |> concat(bagID) |> reduce({Enum, :join, [" "]})

  contain =
    integer(min: 1)
    |> ignore(string(" "))
    |> concat(bag)
    |> ignore(string(" "))
    |> ignore(string("bag"))
    |> ignore(optional(string("s")))
    |> ignore(optional(string(", ")))

  rule =
    bag
    |> ignore(string(" bags contain "))
    |> concat(choice([ignore(string("no other bags")), times(contain, min: 1)]))
    |> ignore(string("."))

  defparsec(:rule, rule)

  def parseRule(s) do
    case s |> rule() do
      {:ok, res, "", _, _, _} ->
        case res do
          [name | contain] ->
            r = {name, contain |> Enum.chunk_every(2) |> Enum.map(fn [a, b] -> {a, b} end)}
            r

          _ ->
            IO.puts("Invalid: " <> s)
        end

      _ ->
        IO.puts("Can't parse: " <> s)
    end
  end
end

# IO.inspect(Parser.parseRule("dotted black bags contain no other bags."))

defmodule Day07 do
  def canContain(rule, rules, want) do
    {_name, contains} = rule

    case Enum.find(contains, fn {_count, name} -> name == want end) do
      nil ->
        # Want not directly in rule, check each sub rule
        Enum.map(contains, fn {_count, name} ->
          case Map.get(rules, name) do
            nil ->
              # Rule not found?
              false

            [] ->
              # Terminating rule, it does not contain want
              false

            xs ->
              # Check sub rule
              canContain({name, xs}, rules, want)
          end
        end)
        |> Enum.any?()

      _ ->
        # Want directly in rule, it can contains
        true
    end
  end

  def canContainInt(rule, rules, want) do
    case canContain(rule, rules, want) do
      true -> 1
      false -> 0
    end
  end

  def countCanContain(rules, want) do
    rules
    |> Map.to_list()
    |> Enum.reduce(0, fn x, acc -> acc + canContainInt(x, rules, want) end)
  end

  def countBags(rules, want) do
    case Map.get(rules, want) do
      nil ->
        0

      [] ->
        0

      xs ->
        xs
        |> Enum.map(fn {count, name} -> count + count * countBags(rules, name) end)
        |> Enum.sum()
    end
  end
end

IO.inspect(
  # examples
  IO.read(:stdio, :all)
  |> String.split("\n", trim: true)
  |> Enum.map(&Parser.parseRule/1)
  |> Map.new()
  #  |> Day07.countCanContain("shiny gold")
  |> Day07.countBags("shiny gold")
)
