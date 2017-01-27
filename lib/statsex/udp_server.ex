defmodule StatsEx.UDPServer do
  @moduledoc """
  Listens to UDP port.
  """
  use GenServer
  require Logger

  import StatsEx.CommandParser, only: [parse: 1]
  import StatsEx.Notifier, only: [notify_data: 1]

  ## API

  def start_link(port) do
    GenServer.start_link(__MODULE__, port, name: __MODULE__)
  end

  ## GenServer Callbacks

  def init(port) do
    :gen_udp.open(port, [:list, {:active, true}])
    {:ok, []}
  end

  def handle_info({:udp, _socket, _ip, _port, packet}, state) do
    Logger.debug("Received new packet: #{Kernel.inspect(packet)}")
    spawn fn -> handle_new_data(packet) end
    {:noreply, state}
  end

  defp handle_new_data(packet) do
    case command = parse(packet) do
      {_bucket, _value, _type} -> notify_data(command)
      :error                   -> Logger.warn("Something went wrong with parsing of the packet: " <> packet)
    end
  end
end
