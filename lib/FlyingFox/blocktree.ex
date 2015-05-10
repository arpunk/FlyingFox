defmodule FlyingFox.Blocktree do

  @initial_coins     Application.get_env :flying_fox, :initial_coins
  @signers_per_block Application.get_env :flying_fox, :signers_per_block

  def blockhash(block) do
    if :data in Dict.keys(block) do block=block[:data] end
    FlyingFox.DetHash.doit(block)
  end

  def genesis_block do
    new = [meta: [revealed: []], data: [height: 0, txs: []]]
    FlyingFox.KV.put("height", 0)
    FlyingFox.KV.put("0", ["genesis"])
    FlyingFox.KV.put("genesis", new)
    genesis_block = [meta: [revealed: []],
                     pub: "BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=",
                     sig: "MEYCIQCu3JqIcIn3jqBhH0nqF8ZTJmdV9GzlJ6WpSq66PA20sAIhAINAuEyCyl2x/iK3BRJM0JGXcd8epnzv0kTX6iHOMAeW",
                     data: [height: 1, txs: [], hash: "z5cVID5hEmZcWNVRmVPRUtSN7e2Z5nXecc+8KWbxk+4=", bond_size: 3.0e11]]
    put_block(genesis_block)
    FlyingFox.KV.put("height", 1)
    FlyingFox.KV.put("1", [blockhash(genesis_block)])
  end

  def sign_reveal do
    FlyingFox.TxCreator.sign
    FlyingFox.TxCreator.reveal
  end

  def put_block(block) do
    height = block[:data][:height]
    IO.puts("height #{inspect height}")
    block_hash = blockhash(block)
    block_hashes = height |> FlyingFox.Blockchain.get_helper
    if block_hashes == nil do block_hashes = [] end
    if block_hash in block_hashes do false else
      block_hashes = block_hashes++[block_hash]
      FlyingFox.KV.put(to_string(height), block_hashes)
      FlyingFox.KV.put(block_hash, Dict.put(block, :meta, [revealed: []]))
      block_hash
    end
  end

  def genesis_state do
    genesis_block
    a = Constants.empty_account # FIXME: Use structs
    ac = @initial_coins
    b = ac/21
    a = Dict.put(a, :amount, 20*b)
    a = Dict.put(a, :bond, b)
    FlyingFox.Keys.master
    creator_pub = FlyingFox.Keys.pubkey
    FlyingFox.KV.put(creator_pub, a)
    FlyingFox.KV.put("tot_bonds", b)
    FlyingFox.KV.put("", [height: 0, hash: ""])
    sign_reveal
  end

  def quick_validation(block) do
    #IO.puts("block #{inspect block}")
    ran = FlyingFox.VerifyTx.rng(block[:data][:hash])
    tot_bonds = FlyingFox.KV.get("tot_bonds")

    l = FlyingFox.Blockchain.txs_filter(block[:data][:txs], "sign")
    |> Enum.map(fn(sign) ->
      acc = FlyingFox.KV.get(sign[:pub])
      Enum.map(sign[:data][:winners], fn(x)->
        FlyingFox.VerifyTx.winner?(acc[:bond], tot_bonds, ran, sign[:pub], x)
      end)
    end) |> Enum.reduce([], &(&1++&2)) |> Enum.map(&(if(&1) do 1 else 0 end)) |> Enum.reduce(0, &(&1+&2))

    cond do
      FlyingFox.Blockchain.winners(block) <= @signers_per_block*2/3 ->
        IO.puts("not enough winners")
        false
      l <= @signers_per_block*1/2 ->
        IO.puts("not enough")
        false
      true -> true
    end
  end

  def enough_validated(blocks, n) do
    cond do
      n == 0 -> true
      blocks == [] -> false
      not quick_validation(hd(blocks)) ->
        IO.puts("bad block")
        false
      true ->
        enough_validated(tl(blocks), n-1)
    end
  end

  def get_height(h) do
    a = FlyingFox.KV.get(h)
    if a == nil do a = [] end
    a
  end

  def add_blocks(blocks) do
    #make sure the networking nodes can pass >30 blocks before calling this function.
    cond do
      blocks == [] -> []
      not :pub in Dict.keys(hd(blocks)) -> add_blocks(tl(blocks))
      not enough_validated(blocks, round(length(get_height(hd(blocks)[:data][:height]))/3)) ->
        #should say "KV.get" in this line!!
        IO.puts("double-signing everywhere")
        false
      FlyingFox.KV.get(blockhash(hd(blocks))) != nil ->
        IO.puts("already have this block")
        add_blocks(tl(blocks))
      true ->
        block_hash = put_block(hd(blocks))
        current_height = KV.get("height")
        if hd(blocks)[:data][:height] > current_height, do: FlyingFox.Blockchain.goto(block_hash)
        add_blocks(tl(blocks))
    end
  end

end
