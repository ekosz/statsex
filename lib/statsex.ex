defmodule StatsEx do
  @moduledoc false
  defmodule State do
    @moduledoc """
    Collected metrics structure.
    """
    defstruct counts: %{}, timers: %{}, gauges: %{}, sets: %{}
  end

  def current_unix_time do
    {mega, secs, _} = :erlang.timestamp()
    mega * 1_000_000 + secs
  end
end
