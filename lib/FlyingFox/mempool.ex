defmodule FlyingFox.Mempool do
  use GenServer

  @name __MODULE__

  #####
  # API

  def start_link do
    GenServer.start_link(__MODULE__, [], name: @name)
  end

  def dump do
    GenServer.cast(@name, :dump)
  end

  def add_tx(tx) do
    GenServer.cast(@name, {:add_tx, tx})
  end

  def txs do
    GenServer.call(@name, :txs)
  end

  #####
  # gen_server callbacks

  def init(args) do
    {:ok, args}
  end

  def handle_cast(:dump, state) do
    {:noreply, state}
  end

  def handle_cast({:add_tx, tx}, state) do
    h = FlyingFox.KV.get("height")
    if h < 1 do prev_hash = nil
    else
      prev_hash = FlyingFox.Blocktree.blockhash(FlyingFox.Blockchain.get_block(h))
    end
    if FlyingFox.VerifyTx.check_tx(tx, x, prev_hash) do x = [tx|x] end
    {:noreply, state}
  end

  def handle_call(:txs, _from, state) do
    {:reply, state, state}
  end

end
