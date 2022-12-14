-module(game_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([init_game/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.

generate_pin(Count, Keys) ->
  generate_pin(Count, Keys, []).
generate_pin(Count, Keys, PIN) when Count > 0 ->
  L = lists:nth(rand:uniform(length(Keys)), Keys),
  generate_pin(Count - 1, Keys, [L|PIN]);
generate_pin(0, _, PIN) ->
  list_to_atom(PIN).

generate_unique_pin() ->
  PIN = generate_pin(4, "ABCDEFGHIGKLMNOPQRSTUVWXYZ23456789"),
  generate_unique_pin(global:whereis_name(PIN), PIN).
generate_unique_pin(undefined, PIN) ->
  PIN;
generate_unique_pin(_, _PIN) ->
  generate_unique_pin().

init_game() ->
  PIN = generate_unique_pin(),
  supervisor:start_child(?MODULE, #{id => PIN, start => {game, start_link, [PIN]}}),
  io:format("GAME SERVER ~p CREATED~n", [PIN]),
  PIN.
