defmodule Glados.Sup do
  use Supervisor.Behaviour

	def start_link() do
	  result = { :ok, sup } = :supervisor.start_link(__MODULE__, [])
	  start_workers(sup)
	  result
	end

	def start_workers(sup) do
	  { :ok, stash } = :supervisor.start_child(sup, worker(Glados.Stash, [ Glados.new ]))
	  { :ok, _     } = :supervisor.start_child(sup, worker(Glados, [ stash ]))
	end

	def init(_) do
	  supervise [], strategy: :one_for_one
	end
end
