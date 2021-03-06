defmodule Blockchain do#the part the blocktree we care about is the blockchain, it ends in the most recent valid block.
  def txs_filter(txs, type) do Enum.filter(txs, fn(t) -> t[:data][:type]==type end) end
  def being_spent(txs) do txs |> txs_filter("spend") |> Enum.map(fn(t) -> t[:data][:amount] end) |> Enum.reduce(0, &(&1+&2)) end
  def prev_block(block) do KV.get(block[:data][:hash]) end
  def valid_block?(block, cost) do 
    #block creator needs to pay a fee. he needs to have signed so we can take his fee.
    f = fn(x) -> x[:data][:bond_size] end
    prev = prev_block(block)
    prev2 = prev_block(prev)
    prev3 = prev_block(prev2)
    min_bond = max(f.(prev), max(f.(prev2), f.(prev3)))
    if min_bond == nil do min_bond = 100000 end
    ngenesis = block[:data][:height]!=1
    cond do
      not is_list(block) -> 
        IO.puts("block should be a dict #{inspect block}")
        false
      min_bond*2/3>f.(block) ->#if the amount of money bonded per block changes too quickly, then it makes it more likely for double-spends to happen.
        IO.puts("not enough bonded")
        false
      ngenesis and prev == nil ->
        IO.puts("blocks come from parents: #{inspect block}")
        IO.puts(inspect block[:data][:height])
        false
      ngenesis and block[:data][:height] - prev[:data][:height] < 1 ->
        IO.puts("cannot redo history")
        false
      not Sign.verify_tx(block) -> 
        IO.puts("bad signature #{inspect block}")
        false
      true ->
        valid_block_2?(block, cost, ngenesis)
    end
  end
  def winners(block) do block[:data][:txs] |> txs_filter("sign") |> Enum.map(&(length(&1[:data][:winners]))) |> Enum.reduce(0, &(&1+&2)) end
  def valid_block_2?(block, cost, ngenesis) do
    wins = winners(block)
    cond do
      ngenesis and wins < Constants.signers_per_block*2/3 -> 
        IO.puts("not enough signers #{inspect wins}")
        IO.puts("block: #{inspect block}")
        false
      not VerifyTx.check_txs(block, cost) ->
        IO.puts("invalid tx")
        false
      true -> valid_block_3?(block, wins) 
    end
  end
  def valid_block_3?(block, ns) do
    txs = block[:data][:txs] 
    sign_txs=txs_filter(txs, "sign")
    signers = Enum.map(sign_txs, fn(t) -> t[:pub] end)
    accs = Enum.map(signers, fn(s) -> KV.get(s) end)
    balances = Enum.map(accs, fn(s) -> s[:bond] end)
    poorest_balance = Enum.reduce(balances, nil, &(min(&1, &2)))
    spending=being_spent(txs)
    bs=block[:data][:bond_size]
    cond do
      poorest_balance < bs -> 
        IO.puts("poorest signer cant afford")
        false
      bs*ns<spending*3 -> 
        IO.puts("not enough bonds to spend that much")
        false
      true -> true
    end
  end
  def get_helper(h) do KV.get(to_string(h)) end#ran 1444 times to add first 2 blocks?!?!
  def get_block(h) do
    if is_integer(h) do h=hd(get_helper(h)) end
    KV.get(h)
  end
  def buy_block(n \\ 1) do
    true = n>0
    height=KV.get("height")
    prev_block = get_block(KV.get("height"))
    txs=Mempool.txs#remove expensive txs until we can afford it. packing problem.
    bh=nil 
    if prev_block != nil do 
      bh=Blocktree.blockhash(prev_block)
    end
    new=[height: height+n, txs: txs, hash: bh, bond_size: 10_000_000_000_000/Constants.signers_per_block*3]#instead of fixed bond size, we shoul look at how big of a bond the txs need.
    new = Keys.sign(new)
    Dict.put(new, :meta, [revealed: []])
  end
  def num_signers(txs) do 
    txs_filter(txs, "sign")
    |> Enum.map(fn(t) -> length(t[:data][:winners]) end) 
    |> Enum.reduce(0, &(&1+&2))
  end
  def back do
    h=KV.get("height")
    if h>0 do
        block = get_block(h)
        prev = get_block(block[:data][:hash])
        txs=block[:data][:txs]
        n=num_signers(txs)
        TxUpdate.txs_updates(txs, -1, round(block[:data][:bond_size] / n))
        TxUpdate.sym_increment(block[:pub], :amount, -Constants.block_creation_fee, -1)
        b = prev[:data][:height]
        if b==nil do b=0 end
        KV.put("height", b)
        Mempool.dump
        true 
    end
  end
  def forward(block) do#while walking forward this needs to reorder the hashes used for get_block so that the block we are using is on top. I thought we only store one of the blockhashes...
    if not is_list(block) do block = KV.get(block) end
    gap = block[:data][:height]-KV.get("height")
    cost = Constants.block_creation_fee*round(:math.pow(2, gap))
    cond do
      not is_list(block) -> [error: "blocks should be lists"]
      KV.get(Blocktree.blockhash(block)) == nil -> [error: "don't have this block"]
      gap < 1 -> [error: "cannot redo history"]
      not valid_block?(block, cost) -> 
        IO.puts("invalid block")
        false      
      true ->
        #block creator needs to pay a fee. he needs to have signed so we can take his fee.
          TxUpdate.sym_increment(block[:pub], :amount, -cost, 1)
          txs=block[:data][:txs]
          n=num_signers(txs)
          TxUpdate.txs_updates(txs, 1, round(block[:data][:bond_size]/n))
          KV.put("height", block[:data][:height])
          Mempool.dump
          hash = Blocktree.blockhash(block)
          n = to_string(block[:data][:height])
          bh = KV.get(n) |> Enum.filter(&(&1!=hash))
          KV.put(n, [hash|bh])
    end
  end
  def goto(hash) do
    h = hash |> get_block
    goto_helper([h])
  end
  def goto_helper(last_blocks) do
    h = KV.get("height")
    if h==0 do 
      my_block = [height: 0]
      hash = ""
    else
      my_block=get_block(h)[:data] 
      hash = Blocktree.blockhash(my_block)
    end
    add_block = hd(last_blocks)[:data]
    cond do
      length(last_blocks)>60 -> 
        IO.puts("error!#! #{inspect last_blocks}")
      hd(last_blocks) == [amount: 0, bond: 0, wait: {0, 0}, nonce: 0] ->
        IO.puts("error 2 #{inspect last_blocks}")
        Enum.map(tl(last_blocks), &(forward(&1)))
      my_block[:height] == 0 or add_block[:hash] == hash -> 
        Enum.map(last_blocks, &(forward(&1)))
      add_block[:height] > my_block[:height] ->
        IO.puts("always here")
        goto_helper([get_block(add_block[:hash])|last_blocks])
      true ->
        IO.puts("back")
        back
        goto_helper(last_blocks)
    end
  end
end
