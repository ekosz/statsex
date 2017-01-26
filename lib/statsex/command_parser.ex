defmodule StatsEx.CommandParser do
  @moduledoc """
  Parses commands in StatsD line format
  """
  @pattern ~r/(?<bucket>\w+):(?<amount>[+-]?\d+)\|(?<type>\w+)/m
  @types ~w(c s g ms)

  def parse(command) do
    captures = Regex.named_captures @pattern, command |> to_string
    with type when type in @types <- captures["type"],
      amount                      <- parse_amount(captures["amount"], type),
      bucket                      <- captures["bucket"]
    do
      {bucket, amount, :"#{type}"}
    else
      _ -> :error
    end
  end

  defp parse_amount(amount, "g"), do: amount

  defp parse_amount(amount, _type) do
    {parsed_amount, ""} = Integer.parse(amount)
    parsed_amount
  end

end
