-module(bkfw).

%% API
-export([start/0]).

%%% API
start() ->
    {ok, _} = application:ensure_all_started(bkfw).
