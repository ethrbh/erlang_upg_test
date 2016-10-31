-module(erlang_upg_test_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	SupFlags = {one_for_one,5,10},
    Child = [{m1,
                     {m1,start_link,[]},
                     transient,10,worker,[m1]}],
	{ok, {SupFlags, Child} }.
