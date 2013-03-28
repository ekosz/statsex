defmodule StatsEx.CommandParser do
  def parse(command) do
    captures = Regex.captures %r/(?<bucket>\w+):(?<amount>[+-]?\d+)\|(?<type>\w+)/g, command
    {captures[:bucket], captures[:amount], captures[:type]}
  end
end
