-module(player).
-behaviour(gen_server).

-include_lib("record.hrl").

%% API.
-export([start_link/1]).
-export([set_client/1, response/2, request/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {uuid, nickname, client, game_pin = undefined}).

%% API.

start_link(UUID) ->
  gen_server:start_link({global, UUID}, ?MODULE, UUID, []).

set_client(UUID) ->
  gen_server:call({global, UUID}, set_client).

response(UUID, Rsp) ->
  gen_server:cast({global, UUID}, Rsp).

request(UUID, Req) ->
  gen_server:cast({global, UUID}, Req).

%% gen_server.

init(UUID) ->
  {ok, #state{uuid = UUID}}.

handle_call(set_client, {Client, _}, State) ->
  {reply, ok, State#state{client = Client}};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(Rsp = #response{}, State) ->
  client:send(State#state.client, Rsp),
  {noreply, State};

handle_cast(#request{action = create}, State) ->
  PIN = game_sup:init_game(),
  client:send(State#state.client, #response{action = create, data = PIN}),
  {noreply, State};

handle_cast(#request{action = join, data = [PIN, Nickname]}, State = #state{game_pin = undefined}) ->
  case game:check_pin(PIN) of
    undefined ->
      client:send(State#state.client, #response{action = join, data = error}),
      {noreply, State};
    GamePIN ->
      NewState = State#state{nickname = Nickname, game_pin = GamePIN},
      game:join(GamePIN, state_to_player(NewState)),
      {noreply, NewState}
  end;

handle_cast(#request{action = join, data = [PIN, Nickname]}, State = #state{game_pin = GamePIN}) ->
  case binary_to_atom(PIN) == GamePIN of
    true ->
      NewState = State#state{nickname = Nickname},
      game:join(GamePIN, state_to_player(NewState)),
      {noreply, NewState};
    false ->
      client:send(State#state.client, #response{action = join, data = error}),
      {noreply, State}
  end;

handle_cast(#request{action = start}, State) ->
  game:start(State#state.game_pin),
  {noreply, State};

handle_cast(#request{action = ready}, State) ->
  game:ready(State#state.game_pin, state_to_player(State)),
  {noreply, State};

handle_cast(#request{action = died}, State) ->
  game:died(State#state.game_pin, state_to_player(State)),
  {noreply, State};

handle_cast(Data, State) ->
  logger:log(debug, "player:handle_cast data: ~p state: ~p", [Data, State]),
  {noreply, State}.

handle_info(Info, State) ->
  logger:log(debug, "player:handle_info info: ~p state: ~p", [Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% private
state_to_player(#state{uuid = UUID, nickname = Nickname}) ->
  #player{uuid = UUID, nickname = Nickname}.
