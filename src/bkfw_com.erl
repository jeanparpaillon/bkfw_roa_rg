-module(bkfw_com).
-author('jean.parpaillon@lizenn.com').

-behaviour(gen_server).

-include("bkfw.hrl").

-export([
    start_link/0,
    subscribe/0,
    send/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(TRACE, "/tmp/bkfw").
-define(EVT_MGR, bkfw_com_events).

-record(state, {
    com :: term(),
    port = undefined :: port(),
    data = <<>> :: binary(),
    crlf,
    trace,
    subscribers
}).

%%%
%%% API
%%%
start_link() ->
    Dev = application:get_env(bkfw, com, undefined),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Dev, []).

subscribe() ->
    gen_server:cast(?MODULE, {subscribe, self()}).

-spec send(Raw :: iolist()) -> ok | {error, Err :: term()}.
send(Raw) ->
    gen_server:cast(?MODULE, {raw, Raw}).

%%%
%%% gen_server callbacks
%%%

-spec init(Com :: string()) -> {ok, term()} | {error, term()} | ignore.
init(undefined) ->
    ?error("COM port undefined"),
    {stop, {undefined_com}};
init(Com) when is_list(Com) ->
    ?info("Opening com port: ~p", [Com]),
    Trace =
        case application:get_env(bkfw, debug, true) of
            true ->
                Tracepath = get_trace_path(0),
                {ok, Dev} = file:open(Tracepath, [append]),
                file:write(Dev, ["[COM] Reopening COM port\n"]),
                Dev;
            false ->
                undefined
        end,
    process_flag(trap_exit, true),
    case cereal:open_tty(Com) of
        {ok, Fd} ->
            Port = open_port({fd, Fd, Fd}, [binary, stream, {line, 80}]),
            {ok, #state{com = Fd, port = Port, trace = Trace, crlf = $\n, subscribers = sets:new()}};
        {error, Err} ->
            ?error("Error opening com port: ~p", [Err]),
            {stop, {error, Err}}
    end;
init(_Args) ->
    ?error("Invalid args: ~p", [_Args]),
    {stop, {error, badarg}}.

handle_call(_, _, S) ->
    {reply, {error, invalid_call}, S}.

handle_cast({subscribe, Pid}, S) ->
    {noreply, S#state{subscribers = sets:add_element(Pid, S#state.subscribers)}};
handle_cast({raw, Data}, #state{port = Port, trace = Trace} = S) ->
    debug_com(Trace, ["[COM] Send raw command: ", Data, "\n"]),
    Port ! {self(), {command, iolist_to_binary(Data)}},
    {noreply, S};
handle_cast({more, Data}, S) ->
    {noreply, S#state{data = Data}}.

handle_info({'EXIT', Pid, _Reason}, S) ->
    {noreply, S#state{subscribers = sets:del_element(Pid, S#state.subscribers)}};
handle_info({Port, {data, {noeol, Bin}}}, #state{port = Port, data = Acc, trace = Trace} = S) ->
    debug_com(Trace, ["[RPI <- CPU] ", Bin]),
    {noreply, S#state{data = <<Acc/binary, Bin/binary>>}};
handle_info({Port, {data, {eol, Bin}}}, #state{port = Port, crlf = $\n, trace = Trace} = S) ->
    debug_com(Trace, ["[RPI <- CPU] ", Bin, "\r"]),
    {noreply, S#state{data = Bin, crlf = $\r}};
handle_info(
    {Port, {data, {eol, Bin}}},
    #state{
        crlf = $\r,
        port = Port,
        data = Data,
        trace = Trace
    } = S
) ->
    debug_com(Trace, ["[RPI <- CPU] ", Bin, "\n"]),
    _ = sets:fold(fun (Pid, _) ->
        Pid ! {data, <<Data/binary, Bin/binary, $\r, $\n>>}
    end, ok, S#state.subscribers),
    {noreply, S#state{ crlf = $\n, data = <<>> }};
handle_info(Info, State) ->
    ?debug("<bkfw_com>Invalid info: ~p (state=~p", [Info, State]),
    {noreply, State}.

terminate(_Reason, #state{com = Fd, port = Port} = _S) ->
    Port ! {self(), close},
    receive
        _ ->
            cereal:close_tty(Fd)
    after 500 ->
        ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Priv
%%%
debug_com(undefined, _) ->
    ok;
debug_com(Dev, Bytes) ->
    Now = erlang:timestamp(),
    {{_Y, _M, _D}, {H, M, S}} = calendar:now_to_universal_time(Now),
    {_, _, MS} = Now,
    file:write(Dev, [io_lib:format("[~p:~p:~p.~p]", [H, M, S, MS]), Bytes]).

get_trace_path(I) ->
    Filename = io_lib:format("~s-~b.trace", [?TRACE, I]),
    case filelib:is_file(Filename) of
        true -> get_trace_path(I + 1);
        false -> Filename
    end.
