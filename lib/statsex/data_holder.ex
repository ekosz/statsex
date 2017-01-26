defmodule StatsEx.DataHolder do
  @moduledoc """
  Holds the collected data until flushed out.
  """
  use GenServer

  import StatsEx.DataCollector, only: [reset: 1, collect: 2]
  import StatsEx.GraphiteFormatter, only: [format: 2]
  import StatsEx.GraphitePusher, only: [send: 1]
  import StatsEx.Notifier, only: [join_feed: 1]

  ## API

  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  ## GenServer Callbacks

  def init([]) do
    join_feed(self())

    {:ok, %StatsEx.State{}}
  end

  def handle_cast({:data, data}, state) do
    {:noreply, collect(data, state)}
  end

  def handle_cast(:flush, state) do
    spawn fn -> flush(state) end
    {:noreply, reset(state)}
  end

  defp flush(state) do
    state
    |> format(:os.system_time(:seconds))
    |> send()
  end
end
