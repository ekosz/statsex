defmodule StatsEx.Supervisor do
  use Supervisor.Behaviour

  def start_link(name, opts) do
    :supervisor.start_link({ :local, name }, __MODULE__, opts)
  end

  @doc """
  Add a child to the given supervisor.
  It accepts the same options as `Supervisor.Behaviour.worker/3`.

  ## Examples

      StatsEx.Supervisor.start_child MySup.supervisor, Worker, []

  """
  def start_child(supervisor, name, args, opts // []) do
    :supervisor.start_child(supervisor, worker(name, args, opts))
  end

  @doc false
  def init(opts) do
    opts = Keyword.put opts, :strategy, :one_for_one
    supervise([], opts)
  end
end
