defmodule StatsEx.GraphiteFormatter do
  @namespace "stats"

  def format(state, timestamp // "") do
    start_time = StatsEx.current_unix_time()
    msg = ""

    msg = format_counts(state.counts, msg, timestamp)
    msg = format_gauges(state.gauges, msg, timestamp)
    msg = format_sets(  state.sets,   msg, timestamp)
    msg = format_timers(state.timers, msg, timestamp)

    msg = calculation_time(start_time - StatsEx.current_unix_time(), msg, timestamp)
    msg
  end

  defp format_counts(counts, msg, timestamp) do
    Enum.reduce counts, msg, fn({key, value}, acc) ->
      acc <> graphite_data_point("#{key}.count", value, timestamp)
    end
  end

  defp format_gauges(gauges, msg, timestamp) do
    Enum.reduce gauges, msg, fn({key, value}, acc) ->
      acc <> graphite_data_point("gauges.#{key}", value, timestamp)
    end
  end

  defp format_sets(sets, msg, timestamp) do
    Enum.reduce sets, msg, fn({key, set}, acc) ->
      acc <> graphite_data_point("sets.#{key}.count", length(set), timestamp)
    end
  end

  defp format_timers(timers, msg, timestamp) do
    Enum.reduce timers, msg, fn({key, timer}, acc) ->
      acc = acc <> graphite_data_point("timers.#{key}.average", timer[:average], timestamp)
      acc = acc <> graphite_data_point("timers.#{key}.lower",   timer[:lower],   timestamp)
      acc = acc <> graphite_data_point("timers.#{key}.upper",   timer[:upper],   timestamp)
      acc = acc <> graphite_data_point("timers.#{key}.sum",     timer[:sum],     timestamp)

      acc <> graphite_data_point("timers.#{key}.standard_deviation", timer[:standard_deviation], timestamp)
    end
  end

  defp calculation_time(time, msg, timestamp) do
    msg <> graphite_data_point("graphiteStats.calculationtime", time, timestamp)
  end

  defp graphite_data_point(key, value, timestamp) do
    "#{@namespace}.#{key} #{value} #{timestamp}\n"
  end

end
