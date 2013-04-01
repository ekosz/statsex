defmodule StatsEx do
  defrecord State, counts: [], timers: [], gauges: [], sets: []

  def current_unix_time do
    {mega, secs, _} = :erlang.now()
    mega * 1_000_000 + secs
  end

  def appvar(var, default) do
    case :application.get_env(:statsex, var) do
      {:ok, value} -> value
      :undefined -> default
    end
  end
end
