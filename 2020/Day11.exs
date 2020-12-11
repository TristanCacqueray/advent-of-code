defmodule Day11 do
  def parse(xs) do
    xs
    |> Enum.map(fn row ->
      row
      |> String.to_charlist()
      |> Enum.map(fn seat ->
        case seat do
          ?. -> :floor
          ?# -> :occupied
          ?L -> :empty
        end
      end)
    end)
  end

  def mapSeat(boat, f) do
    boat
    |> Enum.with_index()
    |> Enum.map(fn {row, y} ->
      row
      |> Enum.with_index()
      |> Enum.map(fn {seat, x} -> seat |> f.({x, y}) end)
    end)
  end

  def print(boat) do
    boat
    |> mapSeat(fn seat, {x, _y} ->
      if x == 0, do: IO.write("\n")

      IO.write(
        case seat do
          :floor -> "."
          :occupied -> "#"
          :empty -> "L"
        end
      )
    end)

    IO.puts("\n======")
  end

  def get(boat, {x, y}) do
    if x < 0 or y < 0,
      do: nil,
      else: boat |> Enum.at(y, []) |> Enum.at(x, nil)
  end

  def occupied?(boat, pos) do
    case boat |> get(pos) do
      :occupied -> true
      _ -> false
    end
  end

  @dir [
    {-1, -1},
    {+0, -1},
    {+1, -1},
    {-1, +0},
    {+1, +0},
    {-1, +1},
    {+0, +1},
    {+1, +1}
  ]

  def adjacent({x, y}) do
    @dir |> Enum.map(fn {dx, dy} -> {x + dx, y + dy} end)
  end

  def updateEmpty1(boat, pos) do
    adjacent(pos) |> Enum.all?(fn p -> not (boat |> occupied?(p)) end)
  end

  def updateOccupied1(boat, pos) do
    adjacent(pos)
    |> Enum.map(fn p -> if boat |> occupied?(p), do: 1, else: 0 end)
    |> Enum.sum() >= 4
  end

  def visible(boat, {x, y}, {dx, dy}) do
    pos = {x + dx, y + dy}

    case boat |> get(pos) do
      :floor -> visible(boat, pos, {dx, dy})
      n -> n
    end
  end

  def visible(boat, pos) do
    @dir |> Enum.map(fn dpos -> visible(boat, pos, dpos) end)
  end

  def updateEmpty2(boat, pos) do
    boat |> visible(pos) |> Enum.all?(fn l -> l != :occupied end)
  end

  def updateOccupied2(boat, pos) do
    boat
    |> visible(pos)
    |> Enum.map(fn l -> if l == :occupied, do: 1, else: 0 end)
    |> Enum.sum() >= 5
  end

  def update(boat, seat, pos) do
    # note: change to updateEmpty1/updateOccupied1 for part1 solution
    case seat do
      :empty -> if boat |> updateEmpty2(pos), do: :occupied, else: :empty
      :occupied -> if boat |> updateOccupied2(pos), do: :empty, else: :occupied
      n -> n
    end
  end

  def update(boat) do
    boat |> mapSeat(fn seat, pos -> boat |> update(seat, pos) end)
  end

  def updateUntilStable(boat) do
    case boat |> update() do
      newBoat when newBoat != boat -> updateUntilStable(newBoat)
      n -> n
    end
  end

  def solve(boat) do
    boat
    |> mapSeat(fn seat, _pos -> if seat == :occupied, do: 1, else: 0 end)
    |> Enum.map(&Enum.sum/1)
    |> Enum.sum()
  end
end

IO.inspect(
  IO.read(:stdio, :all)
  |> String.split("\n", trim: true)
  |> Day11.parse()
  |> Day11.updateUntilStable()
  |> Day11.solve()
)
