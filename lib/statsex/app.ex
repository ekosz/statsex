defmodule StatsEx.App do
  @moduledoc false

  use Application.Behaviour

  def start(_type, _args) do
    {:ok, pid} = StatsEx.Supervisor.start_link(StatsEx.Supervisor, [])

    StatsEx.Supervisor.start_child(StatsEx.Supervisor, StatsEx.Notifier, [])
    :timer.apply_interval(10*1000, StatsEx.Notifier, :notify_flush, [])

    port = StatsEx.appvar(:udp_port, 8888)
    StatsEx.Supervisor.start_child(StatsEx.Supervisor, StatsEx.UDPServer, [port])

    StatsEx.Supervisor.start_child(StatsEx.Supervisor, StatsEx.DataHolder, [])

    {:ok, pid}
  end
end
