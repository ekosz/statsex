defmodule StatsEx.DataHolder do
  use GenServer.Behaviour

  import StatsEx.DataCollector, only: [reset: 1, collect: 2]

  ## API

  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  ## GenServer Callbacks

  def init([]) do
    StatsEx.Notifier.join_feed(self())

    {:ok, StatsEx.State.new}
  end

  def handle_cast({:data, data}, state) do
    {:noreply, collect(data, state)}
  end
  
  def handle_cast(:flush, state) do
    {:noreply, reset(state)}
  end
end