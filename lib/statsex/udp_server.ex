defmodule StatsEx.UDPServer do
  use GenServer.Behaviour

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
    :gen_udp.open(port, [:list, {:active, true}])
    {:noreply, state}
  end

  def handle_info({:udp, _socket, _ip, _port, packet}, state) do
    spawn fn -> handle_new_data(packet) end
    {:noreply, state}
  end

  defp handle_new_data(packet) do
    command = parse(packet)

    case command do
      {bucket, value, type} when !!bucket and !!value and !!type ->
        StatsEx.Notifier.notify_data(command)
      _ ->
    end
  end
end
