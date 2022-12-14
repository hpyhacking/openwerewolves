-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, _Opts) ->
  #{uuid := UUID, nickname := Nickname} = fetch_query(Req),
  {cowboy_websocket, Req, #{uuid => list_to_atom(UUID), nickname => Nickname}}.

websocket_init(State) ->
  #{uuid := UUID, nickname := Nickname} = State,
  %% spawn player process using nickname & uuid
  player_sup:start_player(UUID, Nickname),
  player:to_client(UUID, #{message => <<"Hello World">>}),
  {[], State}.

websocket_handle({text, Json}, State) ->
  #{uuid := UUID} = State,
  Data = jsone:decode(Json),
  player:to_game(UUID, Data),
	{[], State};
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({data, Data}, State) ->
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

