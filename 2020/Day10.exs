defmodule Day10 do
  def reduce1(adapter, acc) do
    case adapter - acc.joltage do
      1 -> %{acc | joltage: adapter, oneInc: acc.oneInc + 1}
      2 -> %{acc | joltage: adapter}
      3 -> %{acc | joltage: adapter, threeInc: acc.threeInc + 1}
      _ -> IO.puts("oops")
    end
  end

  def solve1(xs) do
    result = xs |> Enum.reduce(%{joltage: 0, oneInc: 0, threeInc: 1}, &reduce1/2)
    result.oneInc * result.threeInc
  end

  def return(acc, pos, val) do
    {acc |> Map.put(pos, val), val}
  end

  def testBranch(acc, joltage, value, rest) do
    case value - joltage == 2 or value - joltage == 3 do
      true -> solve2(rest, value, acc)
      false -> {acc, 0}
    end
  end

  def solve2(xs, joltage, acc) do
    case xs do
      [x | rest] ->
        case acc |> Map.get(x) do
          nil ->
            {acc1, c1} = solve2(rest, x, acc)

            case rest do
              [n1, n2 | r] ->
                {acc2, c2} = acc1 |> testBranch(joltage, n1, [n2 | r])
                {acc3, c3} = acc2 |> testBranch(joltage, n2, r)
                acc3 |> return(x, c1 + c2 + c3)

              [n1 | r] ->
                {acc2, c2} = acc1 |> testBranch(joltage, n1, r)
                acc2 |> return(x, c1 + c2)

              [] ->
                acc |> return(x, 1)
            end

          n ->
            {acc, n}
        end

      [] ->
        {acc, 1}
    end
  end

  def solve2(xs) do
    {acc, _} = xs |> solve2(0, Map.new())
    acc |> Map.get(1)
  end
end

IO.inspect(
  IO.read(:stdio, :all)
  |> String.split("\n", trim: true)
  |> Enum.map(fn x -> String.to_integer(x) end)
  |> Enum.sort()
  |> Day10.solve2()
)
