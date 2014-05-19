defmodule Benchmark do

  def log_time(fun) do
    { time, result } = run_and_time(fun)
    IO.puts "Time: #{inspect( time / 1_000_000 )} seconds"
    result
  end

  def time(fun) do
    { time, _ } = run_and_time(fun)
    time
  end

  defp run_and_time(fun) do
      start = :erlang.now
      result = fun.()
      stop = :erlang.now
      time = :timer.now_diff stop, start
      { time, result }
  end
end
