%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(bkfw_mutex).
-export([start_link/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

-define(SRV, ?MODULE).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?SRV, Pid),
    {ok, Pid}.

stop() ->
    ?SRV ! stop.

wait() ->
    ?SRV ! {wait, self()},
    receive ok -> ok end.

signal() ->
    ?SRV ! {signal, self()}, 
    ok.

init() ->
    process_flag(trap_exit, true),
    free().

free() ->
    receive
        {wait, Pid} ->
            link(Pid),
            Pid ! ok,
            busy(Pid);
        stop ->
            terminate()
    end.

busy(Pid) ->
    receive
        {signal, Pid} ->
            unlink(Pid),
            free();
        {'EXIT', Pid, _Reason} ->
            free()
    end.

terminate() ->
    receive
        {wait, Pid} ->
            exit(Pid, kill),
            terminate()
    after
        0 -> ok
    end.
