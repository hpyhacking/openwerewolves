-module(topic).
-behaviour(gen_server).

%% API.
-export([start_link/1, pick/1, count/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {topics}).

%% API.

start_link(PIN) ->
	gen_server:start_link({global, {?MODULE, PIN}}, ?MODULE, [], []).

pick(PIN) ->
  gen_server:call({global, {?MODULE, PIN}}, pick).

count(PIN) ->
  gen_server:call({global, {?MODULE, PIN}}, count).

%% gen_server.

init([]) ->
  Filename = application:get_env(whospy, topics_file, "topics.txt"),
  Topics = [string:split(L, "/") || L <- load(Filename)],
	{ok, #state{topics = Topics}}.

handle_call(pick, _From, State = #state{topics = Topics}) ->
  N = rand:uniform(length(Topics)),
  {L1, L2} = lists:split(N, Topics),
  [[T1, T2]|T] = L2 ++ L1,

  case rand:uniform(100) > 50 of
    true ->
      {reply, [T1, T2], State#state{topics = T}};
    false ->
      {reply, [T2, T1], State#state{topics = T}}
  end;

handle_call(count, _From, State = #state{topics = Topics}) ->
  {reply, length(Topics), State}.

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
