defmodule StatsEx.GraphiteFormatter do
  @moduledoc """
  Formats collected data into format compatible to Graphite.
  """
  @namespace "stats"

  def format(state, timestamp \\ "") do
    start_time = StatsEx.current_unix_time()
    ""
    |> format_counts(state.counts, timestamp)
    |> format_gauges(state.gauges, timestamp)
    |> format_sets(state.sets,   timestamp)
    |> format_timers(state.timers, timestamp)
    |> calculation_time(start_time - StatsEx.current_unix_time(), timestamp)
  end

  defp format_counts(msg, counts, timestamp) do
    Enum.reduce counts, msg, fn({key, value}, acc) ->
      acc <> graphite_data_point("#{key}.count", value, timestamp)
    end
  end

  defp format_gauges(msg, gauges, timestamp) do
    Enum.reduce gauges, msg, fn({key, value}, acc) ->
      acc <> graphite_data_point("gauges.#{key}", value, timestamp)
    end
  end

  defp format_sets(msg, sets, timestamp) do
    Enum.reduce sets, msg, fn({key, set}, acc) ->
      acc <> graphite_data_point("sets.#{key}.count", length(set), timestamp)
    end
  end

  defp format_timers(msg, timers, timestamp) do
    Enum.reduce timers, msg, fn({key, timer}, acc) ->
      key = "timers.#{key}"
      acc = acc <> graphite_data_point("#{key}.average", timer[:average], timestamp)
      acc = acc <> graphite_data_point("#{key}.lower",   timer[:lower],   timestamp)
      acc = acc <> graphite_data_point("#{key}.upper",   timer[:upper],   timestamp)
      acc = acc <> graphite_data_point("#{key}.sum",     timer[:sum],     timestamp)

      acc <> graphite_data_point("#{key}.standard_deviation", timer[:standard_deviation], timestamp)
    end
  end

  defp calculation_time(msg, time, timestamp) do
    msg <> graphite_data_point("graphiteStats.calculationtime", time, timestamp)
  end

  defp graphite_data_point(key, value, timestamp) do
    "#{@namespace}.#{key} #{value} #{timestamp}\n"
  end

end
