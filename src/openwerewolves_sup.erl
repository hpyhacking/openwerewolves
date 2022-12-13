-module(openwerewolves_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  RestartStrategy = {one_for_one, 1, 5},
  ListenerSup = {player_sup, {player_sup, start_link, []}, 
                 permanent, infinity, supervisor, [player_sup]},
	Procs = [ListenerSup],
	{ok, {RestartStrategy, Procs}}.
