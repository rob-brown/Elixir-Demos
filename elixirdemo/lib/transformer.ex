defmodule Transformer do

  # Thirty seconds
  @timeout 30 * 1000

  def start, do: spawn(Transformer, :loop, [])

  def loop do
    receive do
      { :transform, function } ->
        function.()
    end
  end

  def transform(pid, module) do
    send pid, { :transform, &module.loop/0 }
  end

  def rpc(pid, message) do
    send pid, { self, message }
    receive do
      { ^pid, reply } ->
        reply
      after @timeout ->
        :timeout
    end
  end
end

defmodule Transformer.Stack do

  def loop do
    _loop([])
  end
  defp _loop(state) do
    receive do
      { pid, { :push, value }} ->
        send pid, { self, :ok }
        _loop([ value | state ])
      { pid, :pop } ->
        send pid, { self, List.first(state) || :empty }
        _loop(Enum.drop(state, 1))
      { pid, :peek } ->
        send pid, { self, List.first(state) || :empty }
        _loop(state)
      { pid, :state } ->
        send pid, { self, state }
        _loop(state)
      { :transform, function } ->
        function.()
      message ->
        IO.puts "Unknown message: #{inspect message}"
        _loop(state)
    end
  end
end

defmodule Transformer.Fibonacci do

  def loop do
    receive do
      { pid, { :fibonacci, n }} ->
        send pid, { self, fibonacci(n) }
        loop()
      { :transform, function } ->
        function.()
      message ->
        IO.puts "Unknown message: #{inspect message}"
        loop()
    end
  end

  def fibonacci(0), do: 0
  def fibonacci(1), do: 1
  def fibonacci(n), do: fibonacci(n - 1) + fibonacci(n - 2)
end

defmodule Transformer.Fibonacci.Tail do

  def loop do
    receive do
      { pid, { :fibonacci, n }} ->
        send pid, { self, fibonacci(n) }
        loop()
      { :transform, function } ->
        function.()
      message ->
        IO.puts "Unknown message: #{inspect message}"
        loop()
    end
  end

  def fibonacci(n), do: _fibonacci(0, 1, n)
  defp _fibonacci(a, _b, 0), do: a
  defp _fibonacci(a, b, n), do: _fibonacci(b, a + b, n - 1)
end

defmodule Transformer.Fibonacci.Stream do

  def loop do
    receive do
      { pid, { :fibonacci, n }} ->
        send pid, { self, fibonacci(n) }
        loop()
      { :transform, function } ->
        function.()
      message ->
        IO.puts "Unknown message: #{inspect message}"
        loop()
    end
  end

  def fibonacci(n), do: Stream.unfold({0, 1}, fn {a, b} -> {a, {b, a + b}} end) |> Enum.at(n)
end

defmodule Transformer.Factorial do

  def loop do
    receive do
      { pid, { :factorial, n }} ->
        send pid, { self, factorial(n) }
        loop()
      { :transform, function } ->
        function.()
      message ->
        IO.puts "Unknown message: #{inspect message}"
        loop()
    end
  end

  def factorial(n), do: _factorial(n, 1)
  defp _factorial(0, total), do: total
  defp _factorial(1, total), do: total
  defp _factorial(n, total) when n > 1, do: _factorial(n - 1, n * total)
end

defmodule Transformer.Factorial.Reduce do

  def loop do
    receive do
      { pid, { :factorial, n }} ->
        send pid, { self, factorial(n) }
        loop()
      { :transform, function } ->
        function.()
      message ->
        IO.puts "Unknown message: #{inspect message}"
        loop()
    end
  end

  def factorial(0), do: 1
  def factorial(1), do: 1
  def factorial(n) when n > 1, do: Enum.reduce(1..n, 1, &(&1 * &2))
end

defmodule Transformer.Factorial.Parallel do

  def loop do
    receive do
      { pid, { :factorial, n }} ->
        send pid, { self, factorial(n) }
        loop()
      { :transform, function } ->
        function.()
      message ->
        IO.puts "Unknown message: #{inspect message}"
        loop()
    end
  end

  def factorial(0), do: 1
  def factorial(1), do: 1
  def factorial(n) when n > 1, do: Parallel.reduce(1..n, 1, &(&1 * &2))
end

defmodule Transformer.Factorial.Cascade do

  def loop do
    receive do
      { pid, { :factorial, n }} ->
        send pid, { self, factorial(n) }
        loop()
      { :transform, function } ->
        function.()
      message ->
        IO.puts "Unknown message: #{inspect message}"
        loop()
    end
  end

  def factorial(0), do: 1
  def factorial(1), do: 1
  def factorial(n) when n > 1, do: Parallel.cascade_reduce(1..n, 1, &(&1 * &2))
end

defmodule Transformer.Factorial.Hybrid do

  def loop do
    receive do
      { pid, { :factorial, n }} ->
        send pid, { self, factorial(n) }
        loop()
      { :transform, function } ->
        function.()
      message ->
        IO.puts "Unknown message: #{inspect message}"
        loop()
    end
  end

  def factorial(n) when n < 1000, do: Transformer.Factorial.factorial(n)
  def factorial(n), do: Transformer.Factorial.Parallel.factorial(n)
end

defmodule Transformer.Factorial.Benchmark do

  @run_count 3
  @microseconds_per_second 1_000_000
  @modules [
             Transformer.Factorial,
             Transformer.Factorial.Reduce,
             Transformer.Factorial.Parallel,
             Transformer.Factorial.Cascade,
             Transformer.Factorial.Hybrid,
           ]
  @inputs [
                    1,
                   10,
                  100,
                1_000,
               10_000,
              100_000,
            1_000_000,
          ]

  def loop do
    receive do
      { pid, :quick_benchmark } ->
        send pid, { self, quick_profile }
        loop()
      { pid, :benchmark } ->
        send pid, { self, full_profile }
        loop()
      { :transform, function } ->
        function.()
      message ->
        IO.puts "Unknown message: #{inspect message}"
        loop()
    end
  end

  def quick_profile do
    Enum.map(@modules, fn x -> profile(x, 100_000) end)
  end

  def full_profile do
    Enum.map(@modules, fn module ->
                         averages = Enum.map(@inputs, fn n -> profile(module, n) end)
                         { module, averages }
                       end)
  end

  defp profile(module, input) do
    times = Enum.map 1..@run_count, fn _ -> Benchmark.time(fn -> module.factorial input end) end
    total_time = Enum.reduce(times, 0, &(&1 + &2))
    average = total_time / @microseconds_per_second / @run_count
    { input, average }
  end
end
