defmodule FlyingFox.InternalListener do
  use GenServer

  @name __MODULE__

  #####
  # API

  def start_link do
    GenServer.start_link(__MODULE__, [], name: @name)
  end

  #####
  # gen_server callbacks

  def init(args) do
    {:ok, args}
  end

  def handle_cast({type, s, args}, _) do
    spawn_send(s, (fn() -> main(type, args) end))
    {:noreply, []}
  end

  #####
  # Need to remove and fix:

  def export(l, parent) do
    GenServer.cast(key, {hd(l), self(), tl(l) ++ [parent]})
    receive do [:ok, x] -> x end
  end

  def buy_block do
    FlyingFox.BlockAbsorber.buy_block
    FlyingFox.TxCreator.sign
    FlyingFox.TxCreator.reveal
  end

  def main(type, args) do
    case type do
      "buy_block" -> buy_block
      "buy_blocks" -> Enum.map(1..hd(args), fn(_)-> buy_block
                                                    :timer.sleep(1000) end)

      "spend" -> FlyingFox.TxCreator.spend(hd(args), hd(tl(args)))
      "stop" ->
        IO.puts("stopping args")
        send(hd(args), :kill)
      x ->
        IO.puts("is not a command #{inspect x}")
    end
  end

  def spawn_send(s, f) do
    spawn_link(fn() ->
      send s, [:ok, f.()]
    end)
  end

end
