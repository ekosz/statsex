defmodule StatsEx.GraphitePusher do

  def send(payload) do
    host = StatsEx.appvar(:graphite_host, '127.0.0.1')
    port = StatsEx.appvar(:graphite_port, 2003)

    case :gen_tcp.connect(host, port, [:binary, {:packet, 0}]) do
      {:ok, socket} ->
        :gen_tcp.send(socket, payload)
        :gen_tcp.close(socket)
        :ok
      error -> error
    end

  end

end
