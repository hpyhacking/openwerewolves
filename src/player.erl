-module(player).
-behaviour(gen_server).

%% API.
-export([start_link/2]).
-export([set_connection/1, to_client/2, to_server/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {player, nickname, client, game_pin}).

%% API.

start_link(Player, Nickname) ->
  gen_server:start_link({global, Player}, ?MODULE, [Player, Nickname], []).

set_connection(Player) ->
  gen_server:call({global, Player}, set_connection).

to_client(Player, Data) ->
  gen_server:cast({global, Player}, {to_client, Data}).

to_server(Player, #{<<"action">> := Action, <<"data">> := Data}) ->
  gen_server:cast({global, Player}, {list_to_atom(binary_to_list(Action)), Data});
to_server(Player, #{<<"action">> := Action}) ->
  gen_server:cast({global, Player}, {list_to_atom(binary_to_list(Action))}).

%% gen_server.

init([Player, Nickname]) ->
  {ok, #state{player = Player, nickname = Nickname}}.

handle_call(set_connection, {ClientPid, _}, State) ->
  {reply, ok, State#state{client = ClientPid}};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({cast, Data}, State) ->
  erlang:send(State#state.client, {cast, Data}),
  {noreply, State};
handle_cast({create_game}, State = #state{player = _Player}) ->
  PIN = game_sup:init_game(),
  game:join(PIN, Player),
  {noreply, State#state{game_pin = PIN}};
handle_cast({join_game, GamePIN}, State = #{player := Player}) ->
  game:join(GamePIN, Player),
  {noreply, State#state{game_pin = GamePIN}};
handle_cast({ready}, State) ->
  {noreply, State};
handle_cast({died}, State) ->
  {noreply, State};
handle_cast(Data, State) ->
  io:format("data ~p~n", [Data]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
