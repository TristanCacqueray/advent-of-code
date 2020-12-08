example = [
  "nop +0",
  "acc +1",
  "jmp +4",
  "acc +3",
  "jmp -3",
  "acc -99",
  "acc +1",
  "jmp -4",
  "acc +6"
]

defmodule Parser do
  import NimbleParsec

  signed_integer =
    choice([string("+"), string("-")]) |> integer(min: 1) |> post_traverse({:read_integer, []})

  defp read_integer(_rest, args, context, _line, _offset) do
    [val, signStr] = args

    sign =
      case signStr do
        "+" -> 1
        "-" -> -1
      end

    {[val * sign], context}
  end

  nop = ignore(string("nop ")) |> replace(:nop) |> concat(signed_integer)
  acc = ignore(string("acc ")) |> replace(:acc) |> concat(signed_integer)
  jmp = ignore(string("jmp ")) |> replace(:jmp) |> concat(signed_integer)

  defparsec(:instruction, choice([nop, acc, jmp]))

  def parseInstruction(s) do
    case s |> instruction() do
      {:ok, res, "", _, _, _} -> res
      _ -> IO.puts("Can't parse: " <> s)
    end
  end
end

defmodule VM do
  defstruct [:acc, :eix, :instructions]

  def load(input) do
    %VM{acc: 0, eix: 0, instructions: input |> Enum.map(&Parser.parseInstruction/1)}
  end

  def evalAcc(vm, acc) do
    %{vm | acc: vm.acc + acc, eix: vm.eix + 1}
  end

  def evalNop(vm) do
    %{vm | eix: vm.eix + 1}
  end

  def evalJmp(vm, jmp) do
    %{vm | eix: vm.eix + jmp}
  end

  def step(vm) do
    case vm.instructions |> Enum.fetch(vm.eix) do
      {:ok, instruction} ->
        case instruction do
          [:nop, _] -> vm |> evalNop
          [:acc, acc] -> vm |> evalAcc(acc)
          [:jmp, jmp] -> vm |> evalJmp(jmp)
        end

      :error ->
        vm
    end
  end

  def evalUntil(vm, history) do
    newVm = vm |> step

    case newVm.eix >= Enum.count(newVm.instructions) do
      true ->
        {:completed, newVm}

      false ->
        case history |> Enum.member?(newVm.eix) do
          true -> {:stuck, newVm.acc}
          false -> newVm |> evalUntil(history |> Enum.concat([newVm.eix]))
        end
    end
  end

  def evalUntil(vm) do
    vm |> evalUntil([])
  end

  def patchInstruction(instruction) do
    case instruction do
      [:nop, n] -> {:patch, [:jmp, n]}
      [:jmp, n] -> {:patch, [:nop, n]}
      _ -> {:unpatch}
    end
  end

  def patchInstructions(vm, pos) do
    %{
      vm
      | instructions:
          vm.instructions
          |> Enum.map_reduce(0, fn x, acc ->
            case patchInstruction(x) do
              {:patch, newX} ->
                {(acc == pos && newX) || x, acc + 1}

              {:unpatch} ->
                {x, acc}
            end
          end)
          |> elem(0)
    }
  end

  def fixCode(vm, pos) do
    case vm |> patchInstructions(pos) |> evalUntil do
      {:completed, newVm} -> newVm.acc
      {:stuck, _} -> vm |> fixCode(pos + 1)
    end
  end

  def fixCode(vm) do
    vm |> fixCode(0)
  end
end

IO.inspect(
  # example
  IO.read(:stdio, :all)
  |> String.split("\n", trim: true)
  |> VM.load()
  |> VM.fixCode()
)
