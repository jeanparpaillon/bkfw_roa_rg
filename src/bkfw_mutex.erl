%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(bkfw_mutex).

-include("bkfw.hrl").

-export([start_link/0,
		 wait/0,
		 signal/1]).

-export([init/0]).

-define(SRV, ?MODULE).

-define(TIMEOUT, 1000*300).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
	process_flag(trap_exit, true),
    register(?SRV, Pid),
    {ok, Pid}.


-spec wait() -> ok | timeout.
wait() ->
	Mutex = make_ref(),
    ?SRV ! {wait, self(), Mutex},
    receive ok -> Mutex 
	after ?TIMEOUT ->
			?SRV ! {signal, Mutex},
			throw(timeout)
	end.


-spec signal(reference()) -> ok.
signal(Mutex) ->
    ?SRV ! {signal, Mutex},
    ok.


init() ->
    free().


free() ->
    receive
        {wait, Pid, Ref} ->
            link(Pid),
            Pid ! ok,
            busy(Pid, Ref)
    end.


busy(Pid, Ref) ->
    receive
        {signal, Ref} ->
            unlink(Pid),
            free();
		{'EXIT', Pid, _} ->
			unlink(Pid),
			free()
    end.
