-module(player).
-behaviour(gen_server).

%% API.
-export([start_link/2]).
-export([set_connection/1, to_client/2, to_game/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {uuid, nickname, client, game}).

%% API.

start_link(UUID, Nickname) ->
	gen_server:start_link({global, UUID}, ?MODULE, [UUID, Nickname], []).

set_connection(UUID) ->
  gen_server:call({global, UUID}, set_connection).

to_client(UUID, Data) ->
  gen_server:cast({global, UUID}, {to_client, Data}).

to_game(UUID, Data) ->
  gen_server:cast({global, UUID}, {to_game, Data}).

%% gen_server.

init([UUID, Nickname]) ->
	{ok, #state{uuid = UUID, nickname = Nickname}}.

handle_call(set_connection, {ClientPid, _}, State) ->
	{reply, ok, State#state{client = ClientPid}};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({to_client, Data}, State) ->
  erlang:send(State#state.client, {data, Data}),
	{noreply, State};
handle_cast({to_game, _Data}, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
