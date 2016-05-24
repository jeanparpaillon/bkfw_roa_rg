%%%
%%% Monitor EDFA
%%%
-module(bkfw_edfa).
-author('jean.parpaillon@free.fr').

-behaviour(gen_server).

-include("bkfw.hrl").

%%%
%%% API
%%%
-export([start_link/0,
		 stop/0,
		 get_kv/1,
		 loop/1]).

%% SNMP instrumentation
-export([variable_func/2]).

%% Internals
-export([init_loop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(PERIOD, 100).
-define(SLOTS, 128).
%%-define(TID, ?MODULE).

-define(FUNS, [read_n, read_it, read_v, read_a]).

-record(state, {
		  tid,
		  slots                    :: tuple(),
		  curInternalTemp = 0.0,
		  powerSupply = 0.0
		 }).

%%%
%%% API
%%%
start_link() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} ->
			spawn_link(?MODULE, init_loop, []),
			{ok, Pid};
		ignore -> ignore;
		{error, Err} -> {error, Err}
    end.

stop() ->
	gen_server:stop(?MODULE, shutdown, 5000).

init_loop() ->
    loop([ read_infos | ?FUNS ]).

get_kv(1) ->
    [
     {curInternalTemp, get_ets_value(smmCurInternalTemp, 0.0)},
     {powerSupply,     get_ets_value(smmPowerSupply, 0.0)},
     {vendor,          get_ets_value(smmVendor, <<>>)},
     {moduleType,      get_ets_value(smmModuleType, <<>>)},
     {hwVer,           get_ets_value(smmHWVer, <<>>)},
     {hwRev,           get_ets_value(smmHWRev, <<>>)},
     {swVer,           get_ets_value(smmSWVer, <<>>)},
     {fwVer,           get_ets_value(smmFWVer, <<>>)},
     {partNum,         get_ets_value(smmPartNum, <<>>)},
     {serialNum,       get_ets_value(smmSerialNum, <<>>)},
     {productDate,     get_ets_value(smmProductDate, <<>>)}
    ];

get_kv(2) ->
	[
	 {serialnum,       get_ets_value(smmSerialNum, <<>>)},
	 {partnum,         get_ets_value(smmPartNum, <<>>)},
	 {date,            get_ets_value(smmProductDate, <<>>)},
	 {vendor,          case get_ets_value(smmVendor, <<>>) of
						   <<"BKTel Photonics">> -> <<" Bktel\n      Photonics ">>;
						   <<"Laser 2000">> -> <<"   Laser 2000   ">>;
						   <<"Alnair">> -> <<"     Alnair     ">>;
						   <<"Infractive">> -> <<"   Infractive   ">>;
						   _ -> <<"vendor not\nconfigured">>
					   end},
	 {hard,            get_ets_value(smmHWVer, <<>>)},
	 {soft,            get_ets_value(smmSWVer, <<>>)}
	].


%%% SNMP functions
variable_func(new, _) ->
    {value, ok};

variable_func(delete, _) ->
    {value, ok};

variable_func(get, Key) ->
    gen_server:call(?MODULE, {get_snmp, Key}).

%%%
%%% gen_server callbacks
%%%
init([]) ->
    ?info("Starting SMM monitoring", []),
	process_flag(trap_exit, true),
    gen_event:add_handler(bkfw_alarms, bkfw_alarms_snmp, []),
    Tid = ets:new(?MODULE, []),
    ets:insert(Tid, {smmNumber, 0}),
    {ok, #state{tid=Tid, slots=list_to_tuple(lists:duplicate(?SLOTS, false))}}.


handle_call({get_ets_value, Key, Default}, _From, #state{tid=Tid}=S) ->
    Ret = try ets:lookup(Tid, Key) of
			  [{Key, V}] -> V;
			  [] -> Default
		  catch _:_ -> Default
		  end,
    {reply, Ret, S};

handle_call({get_snmp, Key}, _From, #state{tid=Tid}=S) ->
    Ret = case ets:lookup(Tid, Key) of
			  [{Key, V}] when is_float(V) -> {value, round(V)};
			  [{Key, V}] when is_binary(V) -> {value, binary_to_list(V)};
			  [{Key, V}] -> {value, V};
			  [] -> {value, noSuchName}
		  end,
    {reply, Ret, S};

handle_call({call, read_infos}, _From, #state{tid=Tid}=S) ->
    case bkfw_srv:command(0, ri, []) of
		{ok, {0, i, Infos}} ->
			ets:insert(Tid,
					   [
						{smmVendor, get_info(vendor, Infos)},
						{smmModuleType, get_info(moduleType, Infos)},
						{smmHWVer, get_info(hwVer, Infos)},
						{smmHWRev, get_info(hwRev, Infos)},
						{smmSWVer, get_info(swVer, Infos)},
						{smmFWVer, get_info(fwVer, Infos)},
						{smmPartNum, get_info(partNum, Infos)},
						{smmSerialNum, get_info(serialNum, Infos)},
						{smmProductDate, get_info(productDate, Infos)}
					   ]
					  ),
			{reply, ok, S};
		{ok, Ret} ->
			{reply, {error, {string, io_lib:format("RI invalid answer: ~p~n", [Ret])}}, S};
		{error, Err} ->
			{reply, {error, Err}, S}
    end;

handle_call({call, read_v}, _From, #state{tid=Tid}=S) ->
    case bkfw_srv:command(0, rv, []) of
		{ok, {0, v, [V, v]}} when is_float(V); is_integer(V) ->
			ets:insert(Tid, {smmPowerSupply, V}),
			{reply, ok, S#state{powerSupply=V}};
		{ok, Ret} ->
			{reply, {error, {string, io_lib:format("RV invalid answer: ~p~n", [Ret])}}, S};
		{error, Err} ->
			{reply, {error, Err}, S}
    end;

handle_call({call, read_it}, _From, #state{tid=Tid}=S) ->
    case bkfw_srv:command(0, rit, []) of
		{ok, {0, it, [T, <<"C">>]}} when is_float(T); is_integer(T) ->
			ets:insert(Tid, {smmCurInternalTemp, T}),
			{reply, ok, S#state{curInternalTemp=T}};
		{ok, Ret} ->
			{reply, {error, {string, io_lib:format("RIT invalid answer: ~p~n", [Ret])}}, S};
		{error, Err} ->
			{reply, {error, Err}, S}
    end;

handle_call({call, read_n}, _From, #state{tid=Tid}=S) ->
    ets:insert(Tid, {smmNumber, 0}),
    case bkfw_srv:command(0, rn, []) of
		{ok, {0, n, [Mask]}} when is_integer(Mask) ->
												%?debug("Slots: ~p\n", [Mask]),
			handle_slots(Mask, 0, S);
		{ok, Ret} ->
			{reply, {error, {string, io_lib:format("RN invalid answer: ~p~n", [Ret])}}, S};
		{error, Err} ->
			{reply, {error, Err}, S}
    end;

handle_call({call, read_a}, _From, S) ->
    case bkfw_srv:command(0, ra, []) of
		{ok, {0, alarms, Alarms}} ->
			handle_alarms(Alarms, S);
		{ok, Ret} ->
			{reply, {error, {string, io_lib:format("RA invalid answer: ~p~n", [Ret])}}, S};
		{error, Err} ->
			{reply, {error, Err}, S}
    end;

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(shutdown, _State) ->
	?info("Terminate EDFA monitoring", []),
	bkfw_srv:flush(),
    ok;
terminate(_Reason, _State) ->
	ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_info(Key, Infos) ->
    proplists:get_value(Key, Infos, <<>>).

												% loop over all bits of mask and compare with old slots,
												% start or kill bkfw_mon if necessary
handle_slots(_Mask, ?SLOTS, S) ->
    {reply, ok, S};

handle_slots(Mask, Idx, S) when Mask band (1 bsl Idx) == 0 ->
    mnesia:transaction(fun () -> mnesia:delete({ampTable, Idx+1}) end),
    handle_slots(Mask, Idx+1, S);

handle_slots(Mask, Idx, #state{tid=Tid}=S) when Mask band (1 bsl Idx) /= 0 ->
    case mnesia:dirty_match_object(#ampTable{index=Idx+1, _='_'}) of
		[] ->
												% Amp was not there
			Amp = #ampTable{index=Idx+1},
			mnesia:transaction(fun() -> mnesia:write(Amp) end);
		_ ->
												% Amp is already there
			ok
    end,
    ets:insert(Tid, {smmNumber, ets:lookup_element(Tid, smmNumber, 2)+1}),
    handle_slots(Mask, Idx+1, S).


handle_alarms([], S) -> 
    {reply, ok, S};
handle_alarms([Name  | Tail], #state{curInternalTemp=IT, powerSupply=PS}=S) -> 
    gen_event:notify(bkfw_alarms, #smmAlarm{index=0, name=Name, obj={IT, PS}}),
    handle_alarms(Tail, S).


get_ets_value(Key, Default) ->
    try gen_server:call(?MODULE, {get_ets_value, Key, Default}) of
		R -> R
	catch exit:{noproc, _} ->
			Default
	end.

loop([]) ->
    case mnesia:transaction(fun () -> mnesia:first(ampTable) end) of
		{atomic, Key} ->
			loop_mcu(Key),
			timer:sleep(application:get_env(bkfw, edfa_period, ?PERIOD)),
			loop(?FUNS);
		{aborted, Err} ->
			?error("Error reading AMP table: ~p~n", [Err])
    end;
loop([Fun | Tail]) ->
    timer:sleep(application:get_env(bkfw, cmd_period, ?PERIOD)),
    case gen_server:call(?MODULE, {call, Fun}, infinity) of
		ok ->
			loop(Tail);
		{error, timeout} ->
			?debug("SMM ~p timeout. Retrying in ~p ms~n", [Fun, 1000]),
			timer:sleep(1000),
			loop([ Fun | Tail ]);
		{error, {string, Err}} ->
			?error("SMM error: ~s~n", [Err]),
			loop([ Fun | Tail ]);
		{error, Err} ->
			?error("SMM error: ~p~n", [Err]),
			loop([ Fun | Tail ])
    end.

loop_mcu('$end_of_table') ->
    loop(?FUNS);
loop_mcu(Key) ->
    case mnesia:transaction(fun () -> mnesia:read(ampTable, Key) end) of
		{atomic, []} ->
			ok;
		{atomic, [Amp]} -> 
			case bkfw_mcu:loop(Amp) of
				{ok, Amp2} ->
					{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Amp2) end),
					case mnesia:transaction(fun() -> mnesia:next(ampTable, Key) end) of
						{atomic, Key2} -> loop_mcu(Key2);
						{aborted, Err} -> ?error("Error reading AMP table: ~p~n", [Err])
					end;
				{error, timeout} ->
					?error("AMP ~p not responding~n", [Key]),
					loop(?FUNS);
				{error, Err, Amp2} ->
					case Err of
						{string, E} -> ?error("Error monitoring AMP ~p: ~s~n", [Key, E]);
						E -> ?error("Error monitoring AMP ~p: ~p~n", [Key, E])
					end,
					mnesia:transaction(fun() -> mnesia:write(Amp2) end),
					case mnesia:transaction(fun() -> mnesia:next(ampTable, Key) end) of
						{atomic, Key2} -> loop_mcu(Key2);
						{aborted, Err} -> ?error("Error reading AMP table: ~p~n", [Err])
					end
			end
    end.
