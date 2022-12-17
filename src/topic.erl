-module(topic).
-behaviour(gen_server).

%% API.
-export([start_link/0, pick/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {topics}).

%% API.

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

pick() ->
  gen_server:call({global, ?MODULE}, pick).

%% gen_server.

init([]) ->
  Filename = application:get_env(whospy, topics_file, "topics.txt"),
  Topics = [string:split(L, "/") || L <- load(Filename)],
	{ok, #state{topics = Topics}}.

handle_call(pick, _From, State = #state{topics = Topics}) ->
  [T1, T2] = lists:nth(rand:uniform(length(Topics)), Topics),
  case rand:uniform(100) > 50 of
    true ->
      {reply, [T1, T2], State};
    false ->
      {reply, [T2, T1], State}
  end.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

load(FileName) ->
  {ok, Data} = file:read_file(FileName),
  lists:droplast(binary:split(Data, [<<"\n">>], [global])).
