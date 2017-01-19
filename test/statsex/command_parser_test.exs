defmodule CommandParserTest do
  use ExUnit.Case, async: true

  alias StatsEx.CommandParser, as: CommandParser

  @commands [
    [command: "count:1|c",     bucket: "count",   amount: "1",   type: "c"],
    [command: "glork:320|ms",  bucket: "glork",   amount: "320", type: "ms"],
    [command: "gaugor:333|g",  bucket: "gaugor",  amount: "333", type: "g"],
    [command: "gaugor:-10|g",  bucket: "gaugor",  amount: "-10", type: "g"],
    [command: "gaugor:+4|g",   bucket: "gaugor",  amount: "+4",  type: "g"],
    [command: "uniques:765|s", bucket: "uniques", amount: "765", type: "s"]
    ]

  Enum.map @commands, fn(command) ->
    @command command

    test "the bucket of #{@command[:command]} is #{@command[:bucket]}" do
      assert elem(CommandParser.parse(@command[:command]), 0) == @command[:bucket]
    end

    test "the amount in #{@command[:command]} is #{@command[:amount]}" do
      assert elem(CommandParser.parse(@command[:command]), 1) == @command[:amount]
    end

    test "the type of #{@command[:command]} is #{@command[:type]}" do
      assert elem(CommandParser.parse(@command[:command]), 2) == @command[:type]
    end
  end
end
