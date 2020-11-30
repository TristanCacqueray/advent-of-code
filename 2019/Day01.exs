requiredFuel = fn x -> floor(x / 3 - 2) end

IO.read(:stdio, :all)
|> String.split("\n", trim: true)
|> Enum.map(fn x -> String.to_integer(x) end)
|> Enum.map(requiredFuel)
|> Enum.sum()
|> IO.puts()
