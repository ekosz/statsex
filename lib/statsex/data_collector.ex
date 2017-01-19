defmodule StatsEx.DataCollector do
  alias StatsEx.State, as: State

  @plus_sign 43
  @minus_sign 45

  ## API

  def reset(_state) do
    %State{}
  end

  def collect({b, v, t}, state) when is_list(b) do
    collect({:erlang.list_to_atom(b), v, t}, state)
  end

  def collect({b, v, t}, state) when is_list(t) do
    collect({b, v, :erlang.list_to_atom(t)}, state)
  end

  def collect({b, v, :c}, state) do
    count(b, v, state)
  end

  def collect({b, v, :s}, state) do
    set(b, v, state)
  end

  def collect({b, v, :g}, state) do
    gauge(b, v, state)
  end

  def collect({b, v, :ms}, state) do
    time(b, v, state)
  end

  ## Privates

  ### Count

  defp count(b, v, state) when is_list(v) do
    count(b, :erlang.list_to_integer(v), state)
  end

  defp count(bucket, value, state) do
    current_count = state.counts[bucket] || 0

    new_count = current_count + value

    %{state | counts: Keyword.put(state.counts, bucket, new_count)}
  end

  ### Set

  defp set(b, v, state) when is_list(v) do
    set(b, :erlang.list_to_integer(v), state)
  end

  defp set(bucket, value, state) do
    case Enum.member?(state.sets[bucket], value) do
      true ->
        state
      false ->
        current_set = state.sets[bucket] || []
        new_set = current_set ++ [value]
        %{state | sets: Keyword.put(state.sets, bucket, new_set)}
    end
  end

  ### Gauge

  defp gauge(bucket, value, state) when hd(value) == @plus_sign or hd(value) == @minus_sign do
    current_gauge = state.gauges[bucket] || 0

    new_gauge = current_gauge + :erlang.list_to_integer(value)

    %{state | gauges: Keyword.put(state.gauges, bucket, new_gauge)}
  end

  defp gauge(bucket, value, state) do
    %{state | gauges: Keyword.put(state.gauges, bucket, :erlang.list_to_integer(value))}
  end

  ### Time

  defp time(b, v, state) when is_list(v) do
    time(b, :erlang.list_to_integer(v), state)
  end

  defp time(bucket, value, state) do
    timer = state.timers[bucket]
    timer = Keyword.put(timer, :data, timer[:data] ++ [value])

    new_timer = timer
      |> Keyword.put(:mean, mean(timer[:data]))
      |> Keyword.put(:sum, Enum.sum(timer[:data]))
      |> Keyword.put(:upper, :lists.max(timer[:data]))
      |> Keyword.put(:lower, :lists.min(timer[:data]))
      |> Keyword.put(:standard_deviation, standard_dev(timer[:data]))

    %{state | timers: Keyword.put(state.timers, bucket, new_timer)}
  end

  defp mean(array) do
    Enum.sum(array) / length(array)
  end

  defp standard_dev(values) when is_list(values) do
    mean = mean(values)
    :math.sqrt(mean(Enum.map(values, fn(val) -> (val-mean)*(val-mean) end)))
  end
end
