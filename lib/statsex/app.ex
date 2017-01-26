defmodule StatsEx.App do
  @moduledoc """
  Application that spawns an UDP server, a Data Holder and a Notifier
  """
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(StatsEx.Notifier, []),
      worker(StatsEx.UDPServer, [Application.get_env(:statsex, :udp_port)]),
      worker(StatsEx.DataHolder, [])
    ]

    opts = [strategy: :one_for_one, name: StatsEx.Supervisor]
    Supervisor.start_link(children, opts)
  end

end
