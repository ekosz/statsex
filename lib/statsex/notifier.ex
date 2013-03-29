defmodule StatsEx.Notifier do
  @behavior :gen_event

  # API

  def start_link do
    :gen_event.start_link({:local, __MODULE__})
  end

  def join_feed(to_pid) do
    handler_id = {__MODULE__, make_ref()}
    :gen_event.add_sup_handler(__MODULE__, handler_id, [to_pid])
    handler_id
  end

  def notify_data(data) do
    :gen_event.notify(__MODULE__, {:data, data})
  end

  def notify_flush do
    :gen_event.notify(__MODULE__, :flush)
  end

  # GenEvent Callbacks

  def init([pid]) do
    {:ok, pid}
  end

  def handle_event(event, pid) do
    :ok = :gen_server.cast(pid, event)
    {:ok, pid}
  end

  def handle_call(_msg, pid) do
    {:ok, :ok, pid}
  end

  def handle_info(_msg, pid) do
    {:ok, pid}
  end

  def code_change(_old_vsn, pid, _extra) do
    {:ok, pid}
  end
     
  def terminate(_reason, _state) do
    :ok
  end

end

