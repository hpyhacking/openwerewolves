-module(topic_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  ChildSpecs = [#{id => ?MODULE,
                    start => {topic, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [topic]}],
	{ok, {{one_for_one, 1, 5}, ChildSpecs}}.
