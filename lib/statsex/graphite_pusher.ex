defmodule StatsEx.GraphitePusher do
  @moduledoc """
  Pushes data to Graphite.
  """
  require Logger

  def send(payload) do
    Logger.debug("Sending payload: #{payload}")
    host = Application.get_env(:statsex, :graphite_host, '127.0.0.1')
    port = Application.get_env(:statsex, :graphite_port, 2003)

    case :gen_tcp.connect(host, port, [:binary, {:packet, 0}]) do
      {:ok, socket} ->
        :gen_tcp.send(socket, payload)
        :gen_tcp.close(socket)
        :ok
      error ->
        Logger.warn("Error when connecting to host: #{host}, port: #{port}: #{Kernel.inspect(error)}")
        error
    end
  end
end
