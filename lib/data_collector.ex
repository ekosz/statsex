defmodule DataCollector do
  defrecord State, counts: [], timers: [], gauges: [], sets: []

  @plus_sign 43
  @minus_sign 45

  ## API

  def collect({b, v, t}, state) when is_list(b) do
    collect({list_to_atom(b), v, t}, state)
  end

  def collect({b, v, t}, state) when is_list(t) do
    collect({b, v, list_to_atom(t)}, state)
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
    count(b, list_to_integer(v), state)
  end

  defp count(bucket, value, state) do
    current_count = state.counts[bucket] || 0

    count = current_count + value

    state.counts(Keyword.put(state.counts, bucket, count))
  end

  ### Set

  defp set(b, v, state) when is_list(v) do
    set(b, list_to_integer(v), state)
  end

  defp set(bucket, value, state) do
    case List.member?(state.sets[bucket], value) do
      true ->
        state
      false ->
        current_set = state.sets[bucket] || []
        set = current_set ++ [value]
        state.sets(Keyword.put(state.sets, bucket, set))
    end
  end

  ### Gauge

  defp gauge(bucket, value, state) when hd(value) == @plus_sign or hd(value) == @minus_sign do
    current_gauge = state.gauges[bucket] || 0

    gauge = current_gauge + list_to_integer(value)

    state.gauges(Keyword.put(state.gauges, bucket, gauge))
  end

  defp gauge(bucket, value, state) do
    state.gauges(Keyword.put(state.gauges, bucket, list_to_integer(value)))
  end

  ### Time

  defp time(b, v, state) when is_list(v) do
    time(b, list_to_integer(v), state)
  end

  defp time(bucket, value, state) do
    timer = state.timers[bucket]
    timer = Keyword.put(timer, :data, timer[:data] ++ [value])

    timer = Keyword.put(timer, :mean, mean(timer[:data]))
    timer = Keyword.put(timer, :sum, sum(timer[:data]))
    timer = Keyword.put(timer, :upper, :lists.max(timer[:data]))
    timer = Keyword.put(timer, :lower, :lists.min(timer[:data]))
    timer = Keyword.put(timer, :standard_deviation, standard_dev(timer[:data]))

    state.timers(Keyword.put(state.timers, bucket, timer))
  end

  defp sum(array) do
    Enum.reduce(array, 0, fn(unit, acc) -> unit + acc end)
  end

  defp mean(array) do
    sum(array) / length(array)
  end

  defp standard_dev(values) when is_list(values) do
    mean = mean(values)
    :math.sqrt(mean(Enum.map(values, fn(val) -> (val-mean)*(val-mean) end)))
  end
end
