defmodule StatsEx.DataHolder do
  @moduledoc """
  Holds the collected data until flushed out.
  """
  use GenServer

  import StatsEx.DataCollector, only: [collect: 2]
  import StatsEx.GraphiteFormatter, only: [format: 2]
  import StatsEx.GraphitePusher, only: [send: 1]
  import StatsEx.Notifier, only: [join_feed: 1]
  alias StatsEx.State, as: State

  ## API

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
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
    case flush(state) do
      :ok         -> {:noreply, %State{}}
      {:error, _} -> {:noreply, state}
    end
  end

  defp flush(state) do
    state
    |> format(:os.system_time(:seconds))
    |> send()
  end
end
