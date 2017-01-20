defmodule StatsEx.UDPServer do
  use GenServer
  require Logger

  import StatsEx.CommandParser, only: [parse: 1]

  ## API

  def start_link(port) do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [port], [])
  end

  ## GenServer Callbacks

  def init([port]) do
    :gen_server.cast(__MODULE__, {:start_listening, port})
    {:ok, []}
  end

  def handle_cast({:start_listening, port}, state) do
    IO.puts "Handle Cast"
    :gen_udp.open(port, [:list, {:active, true}])
    {:noreply, state}
  end

  def handle_info({:udp, _socket, _ip, _port, packet}, state) do
    IO.puts "Handle Info"
    spawn fn -> handle_new_data(packet) end
    {:noreply, state}
  end

  defp handle_new_data(packet) do
    case command = parse(packet) do
      {_bucket, _value, _type} ->
        StatsEx.Notifier.notify_data(command)
      :error -> Logger.warn("Something went wrong with parsing of the packet: " <> packet)
    end
  end
end
