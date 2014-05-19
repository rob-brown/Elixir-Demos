defmodule Parallel do

  def processor_count do
    :erlang.system_info(:logical_processors_available)
  end

  def scheduler_count do
    :erlang.system_info(:schedulers_online)
  end

  def map(collection, fun) do
    me = self
    collection
      |> Enum.map(fn (elem) ->
           spawn_link fn -> send me, { self, fun.(elem) } end
         end)
      |> Enum.map(fn (pid) ->
           receive do { ^pid, result } -> result end
         end)
  end

  def reduce(collection, acc, fun) do
    chunk_size = Float.ceil(Enum.count(collection) / scheduler_count)
    collection
      |> Enum.chunk(chunk_size, chunk_size, [])
      |> MapReduce.run(acc,
                       fn x, pid ->
                         send pid, { :ok, Enum.reduce(x, acc, fun) }
                       end,
                       fn { _key, values }, local_acc ->
                         Enum.reduce(values, local_acc, fun)
                       end)
  end

  def cascade_reduce(collection, acc, fun) do
    _cascade_reduce(collection, acc, fun, scheduler_count)
  end
  defp _cascade_reduce(collection, _acc, _fun, 0), do: Enum.at(collection, 0)
  defp _cascade_reduce(collection, acc, fun, chunk_count) do
    #IO.puts "Collection size: #{Enum.count collection} chunk count: #{chunk_count}"
    chunk_size = Float.ceil(Enum.count(collection) / chunk_count)
    collection
      |> Enum.chunk(chunk_size, chunk_size, [])
      |> MapReduce.run(acc,
                       fn x, pid ->
                         send pid, { :ok, Enum.reduce(x, acc, fun) }
                       end,
                       fn { _key, values }, _local_acc ->
                         values
                       end)
      |> _cascade_reduce(acc, fun, div(chunk_count, 2))
  end
end
