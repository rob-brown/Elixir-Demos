defmodule Glados.Stash do
  use GenServer.Behaviour

	#####
	# External API

	def start_link(state) do
	  :gen_server.start_link( __MODULE__, state, [] )
	end

	def save_value(pid, value) do
	  :gen_server.cast pid, { :save_value, value }
    value
	end

	def get_value(pid) do
	  :gen_server.call pid, :get_value
	end

	#####
	# Private API

	def init(state) do
	  { :ok, state }
	end

	def handle_cast( { :save_value, value }, _old_state) do
	  { :noreply, value }
	end

	def handle_call( :get_value, _from, state ) do
	  { :reply, state, state }
	end
end
