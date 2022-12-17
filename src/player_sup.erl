-module(player_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_player/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.

start_player(UUID) when is_atom(UUID) ->
  supervisor:start_child(?MODULE, #{id => UUID, start => {player, start_link, [UUID]}}).
