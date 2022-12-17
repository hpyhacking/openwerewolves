-module(client).
-include_lib("record.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-export([send/2]).

%% API

send(Client, Data) ->
  case is_process_alive(Client) of
    true ->
      erlang:send(Client, {cast, Data});
    false ->
      logger:log(debug, "client:response data: ~p", [Data]),
      ignore
  end.

%% Websocket Handler

init(Req, _Opts) ->
  #{uuid := UUID} = fetch_query(Req),
  {cowboy_websocket, Req, #{uuid => list_to_atom(UUID)}}.

websocket_init(State = #{uuid := UUID}) ->
  player_sup:start_player(UUID),
  ok = player:set_client(UUID),
  {[], State}.

websocket_handle({text, <<"ping">>}, State) ->
	{[], State};

websocket_handle({text, JSON}, State = #{uuid := UUID}) ->
  {ok, Req} = decode(JSON),
  player:request(UUID, Req),
	{[], State};

websocket_handle(Data, State) ->
  logger:log(debug, "client:websocket_handle data: ~p state: ~p", [Data, State]),
	{[], State}.

websocket_info({cast, Data}, State) ->
  {ok, JSON} = encode(Data),
	{[{text, JSON}], State};

websocket_info(Info, State) ->
  logger:log(debug, "client:websocket_info info: ~p state: ~p", [Info, State]),
	{[], State}.

%% private

fetch_query(Req) ->
  #{qs := Query} = Req,

  QueryList = lists:map(fun(Item) -> 
                            [Key, Val] = string:split(Item, "="),
                            {binary_to_atom(Key), binary_to_list(Val)}
                        end, string:split(Query, "&")),

  maps:from_list(QueryList).

encode(R = #response{action = Action, data = Data }) ->
  case jsone:try_encode(#{action => Action, data => Data}) of
    {ok, JSON} ->
      {ok, JSON};
    _ ->
      logger:log(debug, "client:encode ~p", [R]),
      error
  end.

decode(JSON) ->
  Opts = [{object_format, map}, {keys, attempt_atom}],
  R = jsone:try_decode(JSON, Opts),

  case R of
    {ok, Data, <<"">>} ->
      {ok, #request{
              action = binary_to_atom(maps:get(action, Data)),
              data = maps:get(data, Data, 'undefined') }};
    _ ->
      logger:log(debug, "client:decode ~p", [JSON]),
      error
  end.

