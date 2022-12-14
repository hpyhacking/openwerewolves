-module(game).
-behaviour(gen_statem).

%% API.
-export([start_link/1]).
-export([join/2, died/2, start/1, ready/2, all_players/1]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([wait/3, started/3]).
-export([terminate/3]).
-export([code_change/4]).

-include_lib("player.hrl").
-record(state, {players = [], ready_players = [], all_players = []}).


-define(HANDLE_COMMON, ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).

-define(MINIMUM_PLAYERS, 4).
-define(MAXIMUM_PLAYERS, 16).


%% API.

start_link(PIN) ->
	gen_statem:start_link({global, PIN}, ?MODULE, [], []).

join(PIN, Player) -> 
	gen_statem:call({global, PIN}, {join, Player}).

ready(PIN, Player) -> 
	gen_statem:cast({global, PIN}, {ready, Player}).

start(PIN) -> 
	gen_statem:cast({global, PIN}, start).

died(PIN, Player) -> 
	gen_statem:cast({global, PIN}, {died, Player}).

all_players(PIN) -> 
	gen_statem:call({global, PIN}, all_players).

%% gen_statem.

callback_mode() ->
	[state_functions, state_enter].

init([]) ->
	{ok, wait, #state{}}.

wait(enter, _OldState, StateData) ->
  io:format("enter wait~n", []),
  {keep_state, StateData#state{players = [], ready_players = []}};

wait(cast, {ready, Player}, StateData = #state{all_players = All, ready_players = Ready}) ->
  case lists:keyfind(Player#player.uuid, #player.uuid, All) of
    false -> 
      {keep_state, StateData};
    ReadyPlayer -> 
      {keep_state, StateData#state{ready_players = [ReadyPlayer | Ready]}}
  end;

wait({call, From}, start, StateData) when length(StateData#state.ready_players) < ?MINIMUM_PLAYERS ->
  {keep_state_and_data, [{reply, From, error}]};

wait({call, From}, start, StateData = #state{ready_players = ReadyPlayers}) ->
  Players = assign_roles(ReadyPlayers),
  {next_state, started, StateData#state{players = Players}, [{reply, From, ok}]};

wait(_EventType, {died, _Player}, StateData) ->
  {keep_state, StateData};

?HANDLE_COMMON.

started(enter, _OldState, _StateData) ->
  io:format("enter started~n", []),
  %% boardcast to all players
  keep_state_and_data;

started(_EventType, {ready, _Player}, _StateData) ->
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
handle_common({call, From}, all_players, StateData = #state{all_players = All}) ->
  {keep_state, StateData, [{reply, From, All}]};
handle_common({call, From}, {join, Player}, StateData = #state{all_players = All}) ->
  case lists:keyfind(Player#player.uuid, #player.uuid, All) of
    false ->
      case length(All) < ?MAXIMUM_PLAYERS of
        true ->
          {keep_state, StateData#state{all_players = [Player|All]}, [{reply, From, ok}]};
        false ->
          {keep_state, StateData, [{reply, From, error}]}
      end;
    _ -> 
      {keep_state, StateData, [{reply, From, ok}]}
  end;
handle_common(Event, EventData, State) ->
  io:format("handle_common ~p ~p ~p~n", [Event, EventData, State]),
  keep_state_and_data.

assign_roles(Players) ->
  %% [Folk, Spy, Fool]
  Plans = [[3,1,0],
           [3,1,1],
           [4,1,1],
           [5,1,1],
           [6,1,1],
           [6,2,1],
           [7,2,1],
           [8,2,1],
           [9,2,1],
           [9,3,1],
           [10,3,1],
           [11,3,1],
           [12,3,1]],
  [[Folk, Spy, Fool]] = [X || X <- Plans, lists:sum(X) =:= length(Players)],

  Disrupted = disrupt(Players),

  Folks = [X#player{role = folk} || X <- lists:sublist(Disrupted, 1, Folk)],
  Spies = [X#player{role = spy}  || X <- lists:sublist(Disrupted, Folk + 1, Spy)],
  Fools = [X#player{role = fool} || X <- lists:sublist(Disrupted, Spy + 1, Fool)],

  disrupt(Folks ++ Spies ++ Fools).

disrupt(Players) ->
  disrupt_by_times(Players, length(Players)).

disrupt_by_times(Players, T) when T > 0 ->
  N = rand:uniform(length(Players)),
  {L1, L2} = lists:split(Players, N),
  disrupt_by_times(L2 ++ L1, T - 1);
disrupt_by_times(Players, 0) ->
  Players.
