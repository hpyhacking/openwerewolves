-module(game).
-behaviour(gen_statem).
-include_lib("record.hrl").

%% API.
-export([start_link/1]).
-export([join/2, died/2, start/1, ready/2, check_pin/1, inspect/2]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([waiting/3, playing/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {roles, playing_players = [], waiting_players = []}).

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

inspect(PIN, Player) -> 
	gen_statem:cast({global, PIN}, {inspect, Player}).

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
  NewStateData = #state{waiting_players = ReplacedPlayers},
  {keep_state, NewStateData, [{state_timeout, 100, broadcast}]};

waiting(state_timeout, broadcast, State = #state{waiting_players = Players}) ->
  Data = state_to_broadcast_waiting(State),
  lists:foreach(broadcast(Data, broadcast_waiting), Players),
  {keep_state_and_data, [{state_timeout, 1000, broadcast}]};

waiting(cast, {ready, #player{uuid = UUID}}, StateData = #state{waiting_players = Players}) ->
  case is_exist_in_waiting(UUID, Players) of
    false -> 
      keep_state_and_data;
    _ -> 
      {keep_state, StateData#state{waiting_players = update_ready(UUID, Players)}}
  end;

waiting(cast, start, StateData) ->
  {C, L} = count_ready_players(StateData#state.waiting_players),
  Players = [X#waiting_player.player || X <- L],

  case match_roles_by_count(C) of
    Roles = [Folk, Spy, Fool] ->
      [FolkTopic, SpyTopic] = topic:pick(),
      PlayingPlayers = shuffle(assign_roles(Folk, Spy, Fool, FolkTopic, SpyTopic, shuffle(Players))),
      {next_state, playing, StateData#state{roles = Roles, playing_players = PlayingPlayers}};
    'undefined' ->
      keep_state_and_data
  end;

waiting(cast, {inspect, _Player}, _StateData) ->
  keep_state_and_data;

waiting(cast, {died, _Player}, _StateData) ->
  keep_state_and_data;

?HANDLE_COMMON.

playing(enter, _OldState, StateData) ->
  logger:log(debug, "game:playing enter state: ~p", [StateData]),
  {keep_state_and_data, [{state_timeout, 500, broadcast}]};

playing(state_timeout, broadcast, State) ->
  Data = state_to_broadcast_playing(State),
  lists:foreach(broadcast(Data, broadcast_playing), State#state.waiting_players),
  keep_state_and_data;

playing(state_timeout, {broadcast_win, Wins}, State) ->
  Data = state_to_broadcast_win(State, Wins),
  lists:foreach(broadcast(Data, broadcast_winning), State#state.waiting_players),
  {next_state, waiting, State};

playing(cast, {ready, #player{uuid = UUID}}, _StateData) ->
  player:response(UUID, #response{action = error, data = playing_ready}),
  keep_state_and_data;

playing(cast, start, _StateData) ->
	keep_state_and_data;

playing(cast, {inspect, #player{uuid = UUID}}, #state{playing_players = Players}) ->
  Topic = inspect_topic(UUID, Players),
  player:response(UUID, #response{action = inspect, data = Topic}),
  keep_state_and_data;

playing(cast, {died, #player{uuid = UUID}}, S = #state{roles = Roles, playing_players = Players}) ->
  case is_exist_in_playing(UUID, Players) of
    false ->
      keep_state_and_data;
    _ ->
      PlayingPlayers = update_died(UUID, Players),

      case which_team_win(Roles, PlayingPlayers) of
        [] ->
          %% no team has won yet
          {keep_state, S#state{playing_players = PlayingPlayers}, [{state_timeout, 500, broadcast}]};
        Wins ->
          {keep_state, S#state{playing_players = PlayingPlayers}, [{state_timeout, 500, {broadcast_win, Wins}}]}
      end
  end;

?HANDLE_COMMON.

which_team_win([_, Spy, Fool], Players) ->
  case count(spy, Players) == 0 of
    true when Fool == 0 -> 
      [folk];
    true -> 
      %% folk team win, maybe fool win too
      %% all spies died
      case count(fool, Players) == Fool of
        true -> [folk, fool];
        false -> [folk]
      end;
    false ->
      case count(folk, Players) == Spy of
        %% spy team win
        %% folks died to equal spies initial
        true -> [spy];
        false -> []
      end
  end.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

% privates
handle_common(cast, {join, P = #player{uuid = UUID, nickname = Nickname}}, StateData = #state{waiting_players = Players}) ->
  %% boardcast game players info to all players client
  case is_exist_in_waiting(UUID, Players) of
    false ->
      case length(Players) < ?MAXIMUM_PLAYERS of
        false ->
          %% can not join the game
          player:response(UUID, ?RSP_JOIN_GAME_ERROR),
          keep_state_and_data;
        true ->
          player:response(UUID, ?RSP_JOIN_GAME_OK),
          NewPlayer = #waiting_player{player = P},
          {keep_state, StateData#state{waiting_players = [NewPlayer|Players]}, [{state_timeout, 500, broadcast}]}
      end;
    _ -> 
      UpdatedPlayers = update_player_nickname(UUID, Nickname, Players),
      player:response(UUID, ?RSP_JOIN_GAME_OK),
      {keep_state, StateData#state{waiting_players = UpdatedPlayers}, [{state_timeout, 500, broadcast}]}
  end;

handle_common(Event, EventData, State) ->
  logger:log(debug, "game:handle_common event: ~p event_data: ~p state: ~p", [Event, EventData, State]),
  keep_state_and_data.

inspect_topic(_, []) -> <<"">>;
inspect_topic(UUID, [H|_]) when H#playing_player.player#player.uuid == UUID ->
  H#playing_player.topic;
inspect_topic(UUID, [_|T]) ->
  inspect_topic(UUID, T).

update_player_nickname(UUID, Nickname, [H|T]) when H#waiting_player.player#player.uuid == UUID ->
  [H#waiting_player{player = H#waiting_player.player#player{nickname = Nickname}} | T];
update_player_nickname(UUID, Nickname, [H|T]) ->
  update_player_nickname(UUID, Nickname, T ++ [H]).

assign_roles(0, 0, 0, _, _, L) -> L;
assign_roles(Folk, Spy, Fool, FolkTopic, SpyTopic, [H|T]) when Folk /= 0 ->
  N = T ++ [#playing_player{role = folk, topic = FolkTopic, player = H}],
  assign_roles(Folk - 1, Spy, Fool, FolkTopic, SpyTopic, N);
assign_roles(Folk, Spy, Fool, FolkTopic, SpyTopic, [H|T]) when Spy /= 0 ->
  N = T ++ [#playing_player{role = spy, topic = SpyTopic, player = H}],
  assign_roles(Folk, Spy - 1, Fool, FolkTopic, SpyTopic, N);
assign_roles(Folk, Spy, Fool, FolkTopic, SpyTopic, [H|T]) when Fool /= 0 ->
  N = T ++ [#playing_player{role = fool, topic = <<"">>, player = H}],
  assign_roles(Folk, Spy, Fool - 1, FolkTopic, SpyTopic, N).

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

is_exist_in_waiting(UUID, WaitingPlayers) ->
  Players = [X#waiting_player.player || X <- WaitingPlayers],
  lists:keyfind(UUID, #player.uuid, Players).

is_exist_in_playing(UUID, PlayingPlayers) ->
  Players = [X#playing_player.player || X <- PlayingPlayers],
  lists:keyfind(UUID, #player.uuid, Players).

update_died(UUID, [H|T]) when UUID == H#playing_player.player#player.uuid ->
  [H#playing_player{is_died = true} | T];
update_died(UUID, [H|T]) ->
  update_died(UUID, T ++ [H]).

broadcast(Data, Action) ->
  fun(#waiting_player{player = #player{uuid = UUID}}) ->
      player:response(UUID, #response{action = Action, data = Data})
  end.

count(Role, Players) ->
  count(Role, Players, 0).
count(_Role, [], Acc) -> Acc;
count(Role, [H|T], Acc) when H#playing_player.is_died == false, H#playing_player.role == Role ->
  count(Role, T, Acc + 1);
count(Role, [_|T], Acc) ->
  count(Role, T, Acc).

state_to_broadcast_win(#state{roles = Roles, playing_players = Players}, Wins) ->
  P = [[{uuid, X#playing_player.player#player.uuid},
        {nickname, X#playing_player.player#player.nickname},
        {role, X#playing_player.role},
        {is_died, X#playing_player.is_died}] || X <- Players],
  #{roles => Roles, players => P, wins => Wins}.

state_to_broadcast_playing(#state{roles = Roles, playing_players = Players}) ->
  P = [[{uuid, X#playing_player.player#player.uuid},
        {nickname, X#playing_player.player#player.nickname},
        {is_died, X#playing_player.is_died}] || X <- Players],
  #{roles => Roles, players => P}.

state_to_broadcast_waiting(#state{waiting_players = Players}) ->
  {C, _} = count_ready_players(Players),

  P = [[{uuid, X#waiting_player.player#player.uuid},
        {nickname, X#waiting_player.player#player.nickname},
        {is_ready, X#waiting_player.is_ready}] || X <- Players],

  case match_roles_by_count(C) of
    'undefined' ->
      #{ready => C, players => P};
    R ->
      #{ready => C, roles => R, players => P}
  end.

match_roles_by_count(C) ->
  case [X || X <- ?ROLES, lists:sum(X) =:= C] of
    [R] -> R;
    [] -> 'undefined'
  end.

count_ready_players(Players) ->
  count_ready_players(Players, 0, []).

count_ready_players([], Acc, L) -> {Acc, L};
count_ready_players([H|T], Acc, L) when H#waiting_player.is_ready == true ->
  count_ready_players(T, Acc + 1, [H|L]);
count_ready_players([_H|T], Acc, L) ->
  count_ready_players(T, Acc, L).
