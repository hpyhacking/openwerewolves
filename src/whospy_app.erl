-module(whospy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Router = [{"/", cowboy_static, {priv_file, whospy, "index.html"}},
						{"/websocket", client, []}],
	Dispatch = cowboy_router:compile([{'_', Router}]),
	Env = #{env => #{dispatch => Dispatch}},

	{ok, _} = cowboy:start_clear(http, [{port, 8080}], Env),

	whospy_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http).
