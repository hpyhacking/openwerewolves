-module(game).
-behaviour(gen_statem).

-include_lib("player.hrl").

%% API.
-export([start_link/1]).
-export([join/2, died/2, start/1, all_players/1]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([wait/3, started/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {players = [], ready_players = [], all_players = []}).

-define(HANDLE_COMMON, ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).

%% API.

start_link(PIN) ->
	gen_statem:start_link({global, PIN}, ?MODULE, [], []).

join(PIN, Player) -> 
  io:format("~p ~p~n", [PIN, Player]),
	gen_statem:cast({global, PIN}, {join, Player}).

start(PIN) -> 
	gen_statem:cast({global, PIN}, start).

died(PIN, Player) -> 
	gen_statem:cast({global, PIN}, {died, Player}).

all_players(PIN) -> 
	gen_statem:call({global, PIN}, all_players).

%% gen_statem.

callback_mode() ->
	state_functions.

init([]) ->
	{ok, wait, #state{}}.

wait(_EventType, {join, Player}, StateData = #state{all_players = AllPlayers}) ->
  {keep_state, StateData#state{all_players = [Player | AllPlayers]}};
wait(_EventType, start, _StateData) ->
	keep_state_and_data;
wait(_EventType, {died, _Player}, _StateData) ->
	keep_state_and_data;
?HANDLE_COMMON.

started(_EventType, {join, _Player}, _StateData) ->
	keep_state_and_data;
started(_EventType, start, _StateData) ->
	keep_state_and_data;
started(_EventType, {died, _Player}, _StateData) ->
	keep_state_and_data;
?HANDLE_COMMON.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

% privates
handle_common({call,From}, all_players, StateData = #state{all_players = AllPlayers}) ->
  {keep_state, StateData, [{reply, From, AllPlayers}]}.
