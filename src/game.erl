-module(game).
-behaviour(gen_statem).

%% API.
-export([start_link/1]).
-export([join/2]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([state_name/3]).
-export([handle_event/4]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {players = []}).

%% API.

start_link(PIN) ->
	gen_statem:start_link({global, PIN}, ?MODULE, [], []).

join(PIN, Player) -> 
	gen_statem:start_link({global, PIN}, ?MODULE, [], []).

%% gen_statem.

callback_mode() ->
	state_functions.

init([]) ->
	{ok, state_name, #state{}}.

state_name(_EventType, _EventData, StateData) ->
	{next_state, state_name, StateData}.

handle_event(_EventType, _EventData, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.