defmodule MapReduce do

  # mapper: fn x, pid -> { key, value }
  # reducer: fn { key, [ values ] }, acc -> newAcc
  def run(collection, acc, mapper, reducer) do
    me = self

    # Spawns a reducer.
    pid = spawn(fn -> reduce(me, collection, acc, mapper, reducer) end)

    # Waits for the reducer to finish.
    receive do
      { ^pid, result } -> result
    end
  end

  defp reduce(pid, collection, acc, mapper, reducer) do

    # Sets the flag on all processes so we receive a (normal) exit message for linked processes.
    Process.flag(:trap_exit, true)
    me = self

    # Spawns a mapper for each item in the collection.
    Enum.each(collection, &(spawn_link(fn -> mapper.(&1, me) end)))

    # Collects the responses and reduces them.
    result = HashDict.new()
               |> collect_replies(Enum.count collection)
               |> Enum.reduce(acc, reducer)

    # Returns the result.
    send pid, { self, result }
  end

  defp collect_replies(result, 0), do: result
  defp collect_replies(result, replies_left) do
    receive do
      # Received a value from a mapper.
      { key, new_val } ->
        case Dict.fetch(result, key) do
          { :ok, values } ->
            result
              |> Dict.put(key, [ new_val | values ])
              |> collect_replies(replies_left - 1)
          :error ->
            result
              |> Dict.put(key, [ new_val ])
              |> collect_replies(replies_left - 1)
        end

      # Exited normally.
      { :EXIT, _from, :normal } ->
        collect_replies(result, replies_left)

      # Exited abnormally.
      # TODO: Respawn the work so the data isn't lost.
      { :EXIT, _from, _reason } ->
        collect_replies(result, replies_left - 1)

      # Unhandled messages.
      msg ->
        IO.puts "Received unknown message: #{inspect msg}"
        collect_replies(result, replies_left)
    end
  end
end
