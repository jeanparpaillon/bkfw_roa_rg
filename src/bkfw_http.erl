-module(bkfw_http).

-include("bkfw.hrl").

%%% API
-export([get_config/0]).

%%% Cowboy REST callbacks
-export([
	 init/3,
	 rest_init/2,
	 allowed_methods/2,
	 content_types_accepted/2,
	 content_types_provided/2,
	 resource_exists/2,
	 allow_missing_post/2,
	 to_json/2
	]).

-define(PORT, 8080).
-define(JSX_OPTS, [{space, 1}, {indent, 2}]).

-record(state, {
	  section   = undefined :: mcu | edfa | sys,
	  index     = undefined :: integer() | undefined | badarg,
	  mcu       = undefined
	 }).

%%%
%%% API
%%%
get_config() ->
    Opts = application:get_env(bkfw, http, []),
    Dir = filename:join(code:priv_dir(bkfw), "www"),
    Handlers = [
		{"/api/mcu/[:index]", bkfw_http, mcu},
		{"/api/edfa",       bkfw_http, edfa},
		{"/api/sys/:name",  bkfw_http, sys},
		{"/[...]", cowboy_static, {dir, Dir, [{mimetypes, cow_mimetypes, all}]}}
	       ],
    Args = [http, 1, 
	    [{port, proplists:get_value(port, Opts, ?PORT)}],
	    [{env, [{dispatch, cowboy_router:compile([{'_', Handlers}])}]},
	     {middlewares, [bkfw_index, cowboy_router, cowboy_handler]}]
	   ],
    {http, {cowboy, start_http, Args}, permanent, 5000, worker, [cowboy]}.

%%%
%%% Cowboy callbacks
%%%
init(_, _, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, mcu) ->
    case cowboy_req:binding(index, Req) of
	{undefined, Req2} ->
	    {ok, Req2, #state{section=mcu, index=undefined}};
	{BinI, Req2} ->
	    try binary_to_integer(BinI) of
		I ->
		    {ok, Req2, #state{section=mcu, index=I}}
	    catch error:badarg ->
		    {ok, Req2, #state{section=mcu, index=badarg}}
	    end
    end;
rest_init(Req, edfa) ->
    {ok, Req, #state{section=edfa}};
rest_init(Req, sys) ->
    {ok, Req, #state{section=sys}};
rest_init(Req, _Sec) ->
    {ok, Req, #state{section=undefined}}.


allowed_methods(Req, State) ->
    {[<<"GET">>,
      <<"HEAD">>,
      <<"POST">>,
      <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, from_json}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, to_json}
     ], Req, State}.

resource_exists(Req, #state{section=mcu, index=badarg}=S) ->
    {false, Req, S};
resource_exists(Req, #state{section=mcu, index=undefined}=S) ->
    {true, Req, S};
resource_exists(Req, #state{section=mcu, index=I}=S) ->
    case mnesia:dirty_match_object(#edfaMcuTable{index=I, _='_'}) of
	[] ->
	    {false, Req, S};
	[Mcu] ->
	    {true, Req, S#state{mcu=Mcu}}
    end    ;
resource_exists(Req, #state{section=undefined}=S) ->
    {false, Req, S};
resource_exists(Req, S) ->
    {true, Req, S}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

to_json(Req, #state{section=mcu, index=undefined}=S) ->
    Mcus = mnesia:dirty_match_object(#edfaMcuTable{_='_'}),
    Ejson = lists:map(fun (M) -> gen_json(mcu, M) end, Mcus),
    {jsx:encode(Ejson, ?JSX_OPTS), Req, S};

to_json(Req, #state{section=mcu, mcu=Mcu}=S) ->
    {jsx:encode(gen_json(mcu, Mcu), ?JSX_OPTS), Req, S};

to_json(Req, #state{section=Sec}=S) ->
    {jsx:encode([{<<"section">>, Sec}], ?JSX_OPTS), Req, S}.


%%%
%%% Internals
%%%
gen_json(mcu, #edfaMcuTable{}=T) ->
    [
     {index,               T#edfaMcuTable.index},
     {ampConsign,          T#edfaMcuTable.ampConsign},
     {gainConsign,         T#edfaMcuTable.gainConsign},
     {outputPowerConsign,  T#edfaMcuTable.outputPowerConsign},
     {operatingMode,       T#edfaMcuTable.operatingMode},
     {curLaserTemp,        T#edfaMcuTable.curLaserTemp},
     {curAmp,              T#edfaMcuTable.curAmp},
     {curInternalAmp,      T#edfaMcuTable.curInternalTemp},
     {powerInput,          T#edfaMcuTable.powerPd1},
     {powerOutput,         T#edfaMcuTable.powerPd2},
     {powerSupply,         T#edfaMcuTable.powerSupply},
     {inputLossThreshold,  T#edfaMcuTable.inputLossThreshold},
     {outputLossThreshold, T#edfaMcuTable.outputLossThreshold},
     {vendor,              T#edfaMcuTable.vendor},
     {moduleType,          T#edfaMcuTable.moduleType},
     {hwVer,               T#edfaMcuTable.hwVer},
     {hwRev,               T#edfaMcuTable.hwRev},
     {swVer,               T#edfaMcuTable.swVer},
     {fwVer,               T#edfaMcuTable.fwVer},
     {partNum,             T#edfaMcuTable.partNum},
     {serialNum,           T#edfaMcuTable.serialNum},
     {productDate,         T#edfaMcuTable.productDate}
    ].
