defmodule Mix.Tasks.Server do
  @moduledoc false
  use Mix.Task
  require Logger

  @shortdoc "Starts applications and their servers"

  def run(_args) do
    Mix.Task.run "app.start", []
    Logger.info("Running StatsEx server")
    unless iex_running?(), do: :timer.sleep(:infinity)
  end

  defp iex_running? do
    Code.ensure_loaded?(IEx) && IEx.started?
  end
end
