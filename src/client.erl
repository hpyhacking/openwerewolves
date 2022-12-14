-module(client).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-export([send/2]).

init(Req, _Opts) ->
  #{uuid := UUID, nickname := Nickname} = fetch_query(Req),
  {cowboy_websocket, Req, #{uuid => list_to_atom(UUID), nickname => Nickname}}.

websocket_init(State = #{uuid := UUID, nickname := Nickname}) ->
  player_sup:start_player(UUID, Nickname),
  {[], State}.

websocket_handle({text, <<"ping">>}, State) ->
	{[], State};
websocket_handle({text, Json}, State = #{uuid := UUID}) ->
  Data = jsone:decode(Json),
  player:to_server(UUID, Data),
	{[], State};
websocket_handle(Data, State) ->
  io:format("~p~n", [Data]),
	{[], State}.

websocket_info({cast, Data}, State) ->
	{[{text, jsone:encode(Data)}], State};
websocket_info(_Info, State) ->
	{[], State}.

send(Client, Data) ->
  erlang:send(Client, {cast, Data}).

fetch_query(Req) ->
  #{qs := Query} = Req,

  QueryList = lists:map(fun(Item) -> 
                            [Key, Val] = string:split(Item, "="),
                            {binary_to_atom(Key), binary_to_list(Val)}
                        end, string:split(Query, "&")),

  maps:from_list(QueryList).

