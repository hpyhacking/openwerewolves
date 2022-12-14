-module(client).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, _Opts) ->
  #{player := Player, nickname := Nickname} = fetch_query(Req),
  {cowboy_websocket, Req, #{player => list_to_atom(Player), nickname => Nickname}}.

websocket_init(State = #{player := Player, nickname := Nickname}) ->
  player_sup:start_player(Player, Nickname),
  {[], State}.

websocket_handle({text, Json}, State = #{player := Player}) ->
  Data = jsone:decode(Json),
  player:to_server(Player, Data),
	{[], State};
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({cast, Data}, State) ->
	{[{text, jsone:encode(Data)}], State};
websocket_info(_Info, State) ->
	{[], State}.

fetch_query(Req) ->
  #{qs := Query} = Req,

  QueryList = lists:map(fun(Item) -> 
                            [Key, Val] = string:split(Item, "="),
                            {binary_to_atom(Key), binary_to_list(Val)}
                        end, string:split(Query, "&")),

  maps:from_list(QueryList).

