defmodule StatsEx.GraphitePusher do

  def send(payload) do
    host = appvar(:graphite_host, '127.0.0.1')
    port = appvar(:graphite_port, 2003)

    case :gen_tcp.connect(host, port, [:binary, {:packet, 0}]) do
      {:ok, socket} ->
        :gen_tcp.send(socket, payload)
        :gen_tcp.close(socket)
        :ok
      error -> error
    end

  end

  defp appvar(var, default) do
    case :application.get_env(:statsex, var) do
      {:ok, value} -> value
      :undefined -> default
    end
  end

end
