defmodule StatsEx.CommandParser do
  def parse(command) do
    captures = Regex.named_captures ~r/(?<bucket>\w+):(?<amount>[+-]?\d+)\|(?<type>\w+)/m, command
    {captures["bucket"], captures["amount"], captures["type"]}
  end
end
