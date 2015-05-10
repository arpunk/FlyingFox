defmodule FlyingFox.Peers do
  #this module is a database of who your peers are, and other
  # data useful for networking that isn't under consensus.
  use GenServer

  @name __MODULE__

  #####
  # API

  def start_link do
    GenServer.start_link(__MODULE__, [], name: @name)
  end

  def get_all do
    GenServer.call(@name, :get_all)
  end

  def get(peer) do
    GenServer.call(@name, {:get, peer})
  end

  def timestamp do
    {_, b, c} = :os.timestamp
    b * 1000 + div(c, 1000)
  end

  def update(peer, height, hash) do
    p = peer |> Dict.put(:time, timestamp) |> Dict.put(:height, height) |> Dict.put(:hash, hash)
    GenServer.cast(@name, {:update, p})
  end

  def new_peer(peer), do: update(peer, 0, "")

  def add_peer(peer) do
    cond do
      is_binary(peer) -> false
      is_integer(peer) -> false
      peer_key(peer) in Dict.keys(get_all) -> false
      true -> new_peer(peer)
    end
  end

  def peer_key(peer) do
    String.to_atom(to_string(peer[:port]) <> "$" <> peer[:ip])
  end

  #####
  # gen_server callbacks

  def init(args) do
    {:ok, args}
  end

  def handle_call(:get_all, _from, state) do
    {:reply, state, state}
  end

  def handle_call({:get, peer}, _from, state) do
    {:reply, Dict.get(state, peer_key(peer)), state}
  end

  def handle_cast({:update, peer}, state) do
    {:noreply, Dict.put(state, peer_key(peer), peer)}
  end

end
