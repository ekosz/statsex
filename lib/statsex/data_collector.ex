defmodule StatsEx.DataCollector do
  alias StatsEx.State, as: State

  @plus_sign 43
  @minus_sign 45
  @types ~w(c s g ms)

  ## API

  def reset(_state) do
    %State{}
  end

  def collect({b, v, :c},  s), do: count(b, v, s)
  def collect({b, v, :s},  s), do: set(  b, v, s)
  def collect({b, v, :g},  s), do: gauge(b, v, s)
  def collect({b, v, :ms}, s), do: time( b, v, s)
  def collect(_, state), do: state

  ## Privates

  ### Count

  defp count(bucket, value, state) do
    current_count = state.counts[bucket] || 0

    new_count = current_count + value

    %{state | counts: Map.put(state.counts, bucket, new_count)}
  end

  ### Set

  defp set(bucket, value, state) do
    case Enum.member?(state.sets[bucket], value) do
      true ->
        state
      false ->
        current_set = state.sets[bucket] || []
        new_set = current_set ++ [value]
        %{state | sets: Map.put(state.sets, bucket, new_set)}
    end
  end

  ### Gauge

  defp gauge(bucket, "+" <> _ = val, state), do: gauge_sum(bucket, Integer.parse(val) |> elem(0), state)
  defp gauge(bucket, "-" <> _ = val, state), do: gauge_sum(bucket, Integer.parse(val) |> elem(0), state)

  defp gauge(bucket, value, state) do
    %{state | gauges: Map.put(state.gauges, bucket, Integer.parse(value) |> elem(0))}
  end

  defp gauge_sum(bucket, value, state) do
    current_gauge = state.gauges[bucket] || 0

    new_gauge = current_gauge + value

    %{state | gauges: Map.put(state.gauges, bucket, new_gauge)}
  end


  ### Time

  defp time(bucket, value, state) do
    timer = state.timers[bucket] || %{data: []}
    timer = Map.put(timer, :data, timer[:data] ++ [value])

    new_timer = timer
      |> Map.put(:mean, mean(timer[:data]))
      |> Map.put(:sum, Enum.sum(timer[:data]))
      |> Map.put(:upper, Enum.max(timer[:data]))
      |> Map.put(:lower, Enum.min(timer[:data]))
      |> Map.put(:standard_deviation, standard_dev(timer[:data]))

    %{state | timers: Map.put(state.timers, bucket, new_timer)}
  end

  defp mean(array) do
    Enum.sum(array) / length(array)
  end

  defp standard_dev(values) when is_list(values) do
    mean = mean(values)
    :math.sqrt(mean(Enum.map(values, fn(val) -> (val-mean)*(val-mean) end)))
  end
end
