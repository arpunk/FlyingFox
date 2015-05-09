defmodule FlyingFox.TxCreator do

  @epoch               Application.get_env :flying_fox, :epoch
  @min_bond            Application.get_env :flying_fox, :min_bond
  @chances_per_address Application.get_env :flying_fox, :chances_per_address

  def nonce(pub) do
    a = FlyingFox.Mempool.txs
        |> Enum.filter(fn(tx) -> tx[:pub] == pub end)
        |> length
    a + FlyingFox.KV.get(pub)[:nonce]
  end

  def spend(amount, to) do
    pub = FlyingFox.Keys.pubkey
    balance = FlyingFox.KV.get(pub)[:amount]

    if balance < amount do
      IO.puts("warning, you cannot afford to spend this tx, so it wont be valid")
    end

    tx = [type: "spend",
          to: to,
          amount: amount,
          nonce: nonce(pub),
          fee: 10000]
    tx = FlyingFox.Keys.sign(tx)
    FlyingFox.Mempool.add_tx(tx)
  end

  def sign do
    pub = FlyingFox.Keys.pubkey
    acc = FlyingFox.KV.get(pub)

    if acc[:bond] > @min_bond do
      h = FlyingFox.KV.get("height")

      if h < 1 do prev_hash = nil else
        prev_hash = FlyingFox.Blocktree.blockhash(Blockchain.get_block(h))
      end

      tot_bonds = FlyingFox.KV.get("tot_bonds")

      w = Enum.filter(0..@chances_per_address, fn(x) ->
        FlyingFox.VerifyTx.winner?(acc[:bond], tot_bonds, FlyingFox.VerifyTx.rng(prev_hash), pub, x)
      end)

      h = FlyingFox.KV.get("height") + 1
      ran = FlyingFox.KV.get("secret #{inspect h}")

      if ran == nil do
        ran = :crypto.rand_bytes(10)
        FlyingFox.KV.put("secret #{inspect h}", ran)
      end

      secret = FlyingFox.DetHash.doit(ran)
      tx = [type: "sign", prev_hash: prev_hash, winners: w, secret_hash: secret, nonce: nonce(pub), height: h-1]
      tx = FlyingFox.Keys.sign(tx)
      FlyingFox.Mempool.add_tx(tx)
    end
  end

  def reveal do
    h = FlyingFox.KV.get("height") - @epoch
    cond do
      h < 2 -> nil
      true -> reveal_2(h)
    end
  end

  def reveal_2(h) do
    pub = FlyingFox.Keys.pubkey
    old_block = FlyingFox.Blockchain.get_block(h)
    old_tx = old_block[:data][:txs]
             |> Enum.filter(&(&1[:data][:type] == "sign"))
             |> Enum.filter(&(&1[:pub] == pub))
             |> hd

    w = old_tx[:data][:winners]
    bond_size = old_block[:data][:bond_size]
    secret = FlyingFox.KV.get("secret #{inspect h}")
    if secret != nil do
      tx = [type: "reveal",
            signed_on: h,
            winners: w,
            amount: length(w) * bond_size,
            secret: FlyingFox.KV.get("secret #{inspect h}"),
            nonce: nonce(pub)]
      tx = FlyingFox.Keys.sign(tx)
      FlyingFox.Mempool.add_tx(tx)
    end
  end

  def slasher(tx1, tx2) do

  end

end
