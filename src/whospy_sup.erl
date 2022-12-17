-module(whospy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  RestartStrategy = {one_for_one, 1, 5},
  PlayerSup = {player_sup, {player_sup, start_link, []}, 
                 permanent, infinity, supervisor, [player_sup]},
  GameSup = {game_sup, {game_sup, start_link, []}, 
                 permanent, infinity, supervisor, [game_sup]},
  TopicSup = {topic_sup, {topic_sup, start_link, []}, 
                 permanent, infinity, supervisor, [topic_sup]},
	Procs = [PlayerSup, GameSup, TopicSup],
	{ok, {RestartStrategy, Procs}}.
