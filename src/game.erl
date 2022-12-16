-module(game).
-behaviour(gen_statem).
-include_lib("record.hrl").

%% API.
-export([start_link/1]).
-export([join/2, died/2, start/1, ready/2, check_pin/1]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([waiting/3, playing/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {playing_players = [], waiting_players = []}).

-define(HANDLE_COMMON, ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).

-define(MINIMUM_PLAYERS, 4).
-define(MAXIMUM_PLAYERS, 16).

%% API.

start_link(PIN) ->
	gen_statem:start_link({global, PIN}, ?MODULE, [], []).

join(PIN, Player) -> 
	gen_statem:cast({global, PIN}, {join, Player}).

ready(PIN, Player) -> 
	gen_statem:cast({global, PIN}, {ready, Player}).

start(PIN) -> 
	gen_statem:cast({global, PIN}, start).

died(PIN, Player) -> 
	gen_statem:cast({global, PIN}, {died, Player}).

check_pin(PIN) when is_binary(PIN) ->
  check_pin(binary_to_atom(PIN));
check_pin(PIN) when is_atom(PIN) ->
  case global:whereis_name(PIN) of
    undefined ->
      undefined;
    _ ->
      PIN
  end.

%% gen_statem.

callback_mode() ->
	[state_functions, state_enter].

init([]) ->
	{ok, waiting, #state{}}.

waiting(enter, _OldState, StateData) ->
  logger:log(debug, "game:waiting enter state: ~p", [StateData]),
  ReplacedPlayers = [X#waiting_player{is_ready = false} || X <- StateData#state.waiting_players],
  {keep_state, StateData#state{playing_players = [], waiting_players = ReplacedPlayers}};

waiting(cast, {ready, #player{uuid = UUID}}, StateData = #state{waiting_players = Players}) ->
  case is_exist(UUID, Players) of
    false -> 
      keep_state_and_data;
    _ -> 
      {keep_state, StateData#state{waiting_players = update_ready(UUID, Players)}}
  end;

waiting(cast, start, StateData) ->
  Players = [P#waiting_player.player || P <- StateData#state.waiting_players, P#waiting_player.is_ready],

  case length(Players) >= ?MINIMUM_PLAYERS of
    true ->
      [[Folk, Spy, Fool]] = [X || X <- ?ROLES, lists:sum(X) =:= length(Players)],
      PlayingPlayers = assign_roles(Folk, Spy, Fool, shuffle(Players)),
      {next_state, playing, StateData#state{playing_players = PlayingPlayers}};
    false -> 
      keep_state_and_data
  end;

waiting(cast, {died, _Player}, StateData) ->
  {keep_state, StateData};

?HANDLE_COMMON.

playing(enter, _OldState, StateData) ->
  logger:log(debug, "game:playing enter state: ~p", [StateData]),
  %% boardcast to all players
  keep_state_and_data;

playing(cast, {ready, #player{uuid = UUID}}, _StateData) ->
  player:response(UUID, #response{action = ready, data = error}),
  keep_state_and_data;

playing(cast, start, _StateData) ->
	keep_state_and_data;

playing(cast, {died, _Player}, _StateData) ->
	keep_state_and_data;

?HANDLE_COMMON.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

% privates
%handle_common(timeout, _, StateData = #state{}) ->
handle_common(cast, {join, P = #player{uuid = UUID}}, StateData = #state{waiting_players = Players}) ->
  %% boardcast game players info to all players client
  case is_exist(UUID, Players) of
    false ->
      case length(Players) < ?MAXIMUM_PLAYERS of
        false ->
          %% can not join the game
          player:response(UUID, ?RSP_JOIN_GAME_ERROR),
          keep_state_and_data;
        true ->
          player:response(UUID, ?RSP_JOIN_GAME_OK),
          NewPlayer = #waiting_player{player = P},
          {keep_state, StateData#state{waiting_players = [NewPlayer|Players]}}
      end;
    _ -> 
      player:response(UUID, ?RSP_JOIN_GAME_OK),
      keep_state_and_data
  end;

handle_common(Event, EventData, State) ->
  logger:log(debug, "game:handle_common event: ~p event_data: ~p state: ~p", [Event, EventData, State]),
  keep_state_and_data.

assign_roles(0, 0, 0, L) -> L;
assign_roles(Folk, Spy, Fool, [H|T]) when Folk /= 0 ->
  N = T ++ [#playing_player{role = folk, player = H}],
  assign_roles(Folk - 1, Spy, Fool, N);
assign_roles(Folk, Spy, Fool, [H|T]) when Spy /= 0 ->
  N = T ++ [#playing_player{role = spy, player = H}],
  assign_roles(Folk, Spy - 1, Fool, N);
assign_roles(Folk, Spy, Fool, [H|T]) when Fool /= 0 ->
  N = T ++ [#playing_player{role = fool, player = H}],
  assign_roles(Folk, Spy, Fool - 1, N).

shuffle(L) ->
  Times = length(L),
  shuffle_by_times(L, Times).

shuffle_by_times(L, 0) -> L;
shuffle_by_times(L, T) ->
  N = rand:uniform(length(L)),
  {L1, L2} = lists:split(N, L),
  shuffle_by_times(L2 ++ L1, T - 1).

update_ready(UUID, [H|T]) when UUID == H#waiting_player.player#player.uuid ->
  [H#waiting_player{is_ready = true} | T];
update_ready(UUID, [H|T]) ->
  update_ready(UUID, T ++ [H]).

is_exist(UUID, WaitingPlayers) ->
  Players = [X#waiting_player.player || X <- WaitingPlayers],
  lists:keyfind(UUID, #player.uuid, Players).


