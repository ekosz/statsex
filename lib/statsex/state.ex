defmodule StatsEx.State do
    @moduledoc """
    Collected metrics structure.
    """
    defstruct counts: %{}, timers: %{}, gauges: %{}, sets: %{}
  end