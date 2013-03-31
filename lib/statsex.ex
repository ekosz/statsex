defmodule StatsEx do
  defrecord State, counts: [], timers: [], gauges: [], sets: []

  def current_unix_time do
    {mega, secs, _} = :erlang.now()
    mega * 1_000_000 + secs
  end
end
