defmodule FlyingFox.BlockAbsorber do
  use GenServer

  @name __MODULE__

  #####
  # API

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  def absorb(blocks) do
    GenServer.call(@name, {:blocks, blocks})
  end

  def buy_block do
    GenServer.call(@name, {:blocks, [FlyingFox.Blockchain.buy_block]})
  end

  #####
  # gen_server callbacks

  def init(_args) do
    FlyingFox.Blocktree.genesis_state
    FlyingFox.Keys.master
    {:ok, []}
  end

  def handle_call({:blocks, blocks}, _from, state) do
    FlyingFox.Blocktree.add_blocks(blocks)
    {:reply, :ok, state}
  end

  #####
  # Internal functions

  # FIXME: Lookup if this function needs to be exported and/or if it can
  # be private instead
  def buy_blocks(n) do
    Enum.map(1..n, fn(_) ->
      :timer.sleep(1000)
      FlyingFox.BlockAbsorber.buy_block
      FlyingFox.TxCreator.sign
      FlyingFox.TxCreator.reveal
    end)
  end
end
