defmodule StatsEx.App do
  @moduledoc false

  use Application.Behaviour

  def start(_type, _args) do
    {:ok, pid} = StatsEx.Supervisor.start_link(StatsEx.Supervisor, [])
    StatsEx.Supervisor.start_child(StatsEx.Supervisor, StatsEx.Notifier, [])
    StatsEx.Supervisor.start_child(StatsEx.Supervisor, StatsEx.UDPServer, [8888])
    StatsEx.Supervisor.start_child(StatsEx.Supervisor, StatsEx.DataHolder, [])
    {:ok, pid}
  end
end
