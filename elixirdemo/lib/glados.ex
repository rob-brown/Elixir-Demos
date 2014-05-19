defmodule Glados do
  use GenServer.Behaviour

  alias Glados.Stash, as: Stash

  @default_message "Welcome to Aperture Laboratories."

  defstruct quotes: [], startup_message: @default_message, quote_count: 0, incinerate_count: 0, sing_count: 0, paradox_count: 0, crash_count: 0

  #####
  # Public API

  def new(), do: %Glados{quotes: read_quotes()}

  def start_link(stash) do
    glados = Stash.get_value stash
    :gen_server.start_link( { :local, :glados }, __MODULE__, [ glados, stash ], [] )
  end

  def stats(),      do: :gen_server.call(:glados, :stats)
  def insult(),     do: :gen_server.call(:glados, :insult)
  def sing(),       do: :gen_server.call(:glados, :sing)
  def incinerate(), do: :gen_server.cast(:glados, :incinerate)
  def paradox(),    do: :gen_server.cast(:glados, :paradox)
  def crash(),      do: :gen_server.cast(:glados, :crash)

  #####
  # Private API

  def init(state = [ glados, _stash ]) do
    IO.puts glados.startup_message
    { :ok, state }
  end

  def handle_call( :stats, _from, [ glados, stash ] ) do
    stats = "Insults: #{glados.quote_count} Incinerations: #{glados.incinerate_count} Songs: #{glados.sing_count} Paradoxes: #{glados.paradox_count} Crashes: #{glados.crash_count}"
    { :reply, stats, [ glados, stash ] }
  end

  def handle_call( :insult, _from, [ glados, stash ] ) do
    { insult, new_glados } = random_quote glados, stash
    { :reply, insult, [ new_glados, stash ] }
  end

  def handle_call( :sing, _from, [ glados, stash ] ) do
    { song, new_glados } = sing glados, stash
    { :reply, song, [ new_glados, stash ] }
  end

  def handle_cast( :incinerate, [ glados, stash ]) do
    new_glados = incinerate glados, stash
    { :noreply, [ new_glados, stash ] }
  end

  def handle_cast( :crash, [ glados, stash ]) do
    new_glados = crash glados, stash
    { :noreply, [ new_glados, stash ] }
  end

  def handle_cast( :paradox, [ glados, stash ]) do
    new_glados = paradox glados, stash
    { :noreply, [ new_glados, stash ] }
  end

  def format_status(_reason, [ _pdict, [ _glados, _stash ] ]) do
    # Used for printing debug info about the GenServer.
    # For the demo, a minimal string is returned to reduce the size of error reports.
    [data: [{'State', "GLaDOS debug info..."}]]
  end

  def terminate(_reason, [ _glados, _stash ]) do
    # This is a great place to stash the state.
    # However, for this example, we stash before intentionally crashing.
    IO.puts "GLaDOS died."
  end

  #####
  # Helper methods

  defp crash(glados, stash) do
    message = """
    GLaDOS: You think you're doing some damage? Two plus two is...
    [sparking and fizzling noise]
    GLaDOS: Ten. IN BASE FOUR! I'M FINE!
    """
    new_glados = %{ glados | startup_message: message, crash_count: glados.crash_count + 1 }
    Stash.save_value stash, new_glados
    raise RuntimeError, message: "Crashing"
    new_glados
  end

  defp incinerate(glados, stash) do
    message = "Do you know the biggest lesson I learned from what you did? I discovered I have a sort of black box quick-save feature. In the event of a catastrophic failure, the last two minutes of my life are preserved for analysis. I was able - well, forced really - to relive you killing me. Again and again. Forever. You know, if you'd done that to somebody else, they might devote their existence to exacting REVENGE! Luckily, I'm a bigger person than that. I'm happy to put this all behind us and get back to work. After all, we've got a lot to do, and only sixty more years to do it. More or less. I don't have the actuarial tables in front of me."
    new_glados = %{ glados | startup_message: message, incinerate_count: glados.incinerate_count + 1 }
    Stash.save_value stash, new_glados
    exit(:incinerate)
    new_glados
  end

  defp paradox(glados, stash) do
    message = "This. Sentence. Is. FALSE."
    new_glados = %{ glados | startup_message: message, paradox_count: glados.paradox_count + 1 }
    Stash.save_value stash, new_glados
    _ = 1 / 0
    new_glados
  end

  defp sing(glados, stash) do
    song = """
    This was a triumph!
    I'm making a note here:
    "huge success!!"

    It's hard to overstate
    My satisfaction.

    Aperture science:
    We do what me must
    Because we can.

    For the good of all of us.
    Except the ones who are dead.

    But there's no sense crying
    Over every mistake.
    You just keep on trying
    Till you run out of cake.
    And the science gets done.
    And you make a neat gun
    For the people who are
    Still alive.

    I'm not even angry...
    I'm being so sincere right now-
    Even though you broke my heart,
    And killed me.

    And tore me to pieces.
    And threw every piece into a fire.
    As they burned it hurt because
    I was so happy for you!

    Now, these points of data
    Make a beautiful line.
    And we're out of beta.
    We're releasing on time!
    So i'm glad i got burned-
    Think of all the things we learned-
    For the people who are
    Still alive.

    Go ahead and leave me...
    I think i'd prefer to stay inside...
    Maybe you'll find someone else
    To help you?
    Maybe black mesa?
    That was a joke! haha!! fat chance!!

    Anyway this cake is great!
    It's so delicious and moist!

    Look at me: still talking
    When there's science to do!
    When i look out there,
    It makes me glad i love you.

    I've experiments to run.
    There is research to be done.
    On the people who are
    Still alive.
    And believe me i am
    Still alive.
    I'm doing science and i'm
    Still alive.
    I feel fantastic and i'm
    Still alive.
    While you're dying i'll be
    Still alive.
    And when you're dead i will be
    Still alive.

    Still alive.

    Still alive.
    """
    new_glados = Stash.save_value stash, %{ glados | sing_count: glados.sing_count + 1 }
    { song, new_glados }
  end

  defp random_quote(glados, stash) do
    count = Enum.count glados.quotes
    quote = Enum.at glados.quotes, :random.uniform(count)
    new_glados = Stash.save_value stash, %{ glados | quote_count: glados.quote_count + 1 }
    { quote, new_glados }
  end

  defp read_quotes() do
    read_quote_file() |> parse_quotes()
  end

  defp read_quote_file() do
    :filename.join([ __DIR__, "glados", "glados.txt" ]) |> File.read!
  end

  defp parse_quotes(quotes) do
    quotes |> String.split "\n\n"
  end
end

defimpl Inspect, for: Glados do
  def inspect(_glados, _opts) do
    "GLaDOS"  # Printing the GLaDOS struct is too big due to the quotes. So just prints a string.
  end
end
