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
		 to_json/2,
		 from_json/2,
		 from_multipart/2,
		 is_authorized/2
		]).

-define(PORT, 8080).
-define(JSX_OPTS, [{space, 1}, {indent, 2}]).
-define(MAX_SIZE, 1024*1024*1024*50).
-define(REALM, <<"bkfw">>).

-define(set_error(E, Req), set_errors([E], Req)).
-define(set_errors(E, Req), set_errors(E, Req)).

-record(state, {
		  section   = undefined :: mcu | edfa | sys | {firmware, string()},
		  index     = undefined :: integer() | undefined | badarg,
		  mcu       = undefined,
		  sys       = undefined :: login | net | password | community | usm | protocol | 
								   reset | reboot | targets | usb,
		  firmware  = undefined :: undefined | string(),
		  version   = 1         :: integer()
		 }).

%%%
%%% API
%%%
get_config() ->
    Opts = application:get_env(bkfw, http, []),
    Dir = filename:join(code:priv_dir(bkfw), "www"),
    DftLogo = filename:join(code:priv_dir(bkfw), "logo.png"),
    Logo = application:get_env(bkfw, logo, DftLogo),
    Handlers = [
				{"/api/mcu/[:index]",       bkfw_http, {1, mcu}},
				{"/api2/mcu/[:index]",      bkfw_http, {2, mcu}},
				{"/api/edfa",               bkfw_http, {1, edfa}},
				{"/api2/edfa",              bkfw_http, {2, edfa}},
				{"/api/alarms",             bkfw_http_ws, []},
				{"/api/sys/firmware/[:fw]", bkfw_http, firmware},
				{"/api/sys/:name",          bkfw_http, sys},
				{"/api2/params",            bkfw_http, params},
				{"/logo",                   cowboy_static, {file, Logo, [{mimetypes, cow_mimetypes, all}]}},
				{"/[...]",                  cowboy_static, {dir, Dir, [{mimetypes, cow_mimetypes, all}]}}
			   ],
    Args = [http, 1,
			[{port, proplists:get_value(port, Opts, ?PORT)}],
			[{env, [{dispatch, cowboy_router:compile([{'_', Handlers}])}]},
			 {middlewares, [bkfw_index, cowboy_router, cowboy_handler]}]
		   ],
    {bkfw_http, {cowboy, start_http, Args}, permanent, 5000, worker, [cowboy]}.

%%%
%%% Cowboy callbacks
%%%
init(_, _Req, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, {Version, mcu}) ->
    case cowboy_req:binding(index, Req) of
		{undefined, Req2} ->
			{ok, Req2, #state{section=mcu, index=undefined, version=Version}};
		{BinI, Req2} ->
			try binary_to_integer(BinI) of
				I ->
					{ok, Req2, #state{section=mcu, index=I, version=Version}}
			catch error:badarg ->
					{ok, Req2, #state{section=mcu, index=badarg, version=Version}}
			end
    end;
rest_init(Req, {Version, edfa}) ->
    {ok, Req, #state{section=edfa, version=Version}};
rest_init(Req, params) ->
	{ok, Req, #state{section=params}};
rest_init(Req, sys) ->
    case cowboy_req:binding(name, Req) of
		{<<"login">>, Req2} -> {ok, Req2, #state{section=sys, sys=login}};
		{<<"reset">>, Req2} -> {ok, Req2, #state{section=sys, sys=reset}};
		{<<"reboot">>, Req2} -> {ok, Req2, #state{section=sys, sys=reboot}};
		{<<"net">>, Req2} -> {ok, Req2, #state{section=sys, sys=net}};
		{<<"network">>, Req2} -> {ok, Req2, #state{section=sys, sys=net}};
		{<<"password">>, Req2} -> {ok, Req2, #state{section=sys, sys=password}};
		{<<"community">>, Req2} -> {ok, Req2, #state{section=sys, sys=community}};
		{<<"usm">>, Req2} -> {ok, Req2, #state{section=sys, sys=usm}};
		{<<"targets">>, Req2} -> {ok, Req2, #state{section=sys, sys=targets}};
		{<<"protocol">>, Req2} -> {ok, Req2, #state{section=sys, sys=protocol}};
		{<<"usb">>, Req2} -> {ok, Req2, #state{section=sys, sys=usb}};
		{_, Req2} -> {ok, Req2, #state{section=sys, sys=undefined}}
    end;
rest_init(Req, firmware) ->
	%% /api/sys/firmware/[fw|cpu|amp]
    case cowboy_req:binding(fw, Req) of
		{<<"fw">>, Req2} -> {ok, Req2, #state{section={firmware, "fw"}}};
		{<<"cpu">>, Req2} -> {ok, Req2, #state{section={firmware, "cpu"}}};
		{<<"amp">>, Req2} -> {ok, Req2, #state{section={firmware, "amp"}}};
		{_, Req2} -> {ok, Req2, #state{section=firmware}}
    end;	
rest_init(Req, _Sec) ->
    {ok, Req, #state{section=undefined}}.


allowed_methods(Req, #state{section=edfa}=S) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, S};
allowed_methods(Req, #state{section=firmware}=S) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, S};
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"POST">>, <<"OPTIONS">>], Req, State}.


content_types_accepted(Req, #state{section={firmware, _}}=State) ->
    {[
      {{<<"multipart">>, <<"form-data">>, '*'}, from_multipart}
     ], Req, State};
content_types_accepted(Req, State) ->
    case cowboy_req:has_body(Req) of
		true ->
			{[
			  {{<<"application">>, <<"json">>, '*'}, from_json}
			 ], Req, State};
		false ->
			{[], Req, State}
    end.


content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, to_json}
     ], Req, State}.


is_authorized(Req, #state{version=2}=State) ->
    require_auth(Req, State);

is_authorized(Req, #state{section=sys, sys=login}=State) ->
    {true, Req, State};

is_authorized(Req, #state{section=Sec}=State) when Sec =:= sys orelse Sec =:= firmware ->
    require_auth(Req, State);

is_authorized(Req, #state{section={firmware, _}}=State) ->
    require_auth(Req, State);

is_authorized(Req, State) ->
    case cowboy_req:method(Req) of
		{<<"POST">>, Req2} ->
			require_auth(Req2, State);
		{_, Req2} ->
			{true, Req2, State}
    end.


resource_exists(Req, #state{section=mcu, index=badarg}=S) ->
    {false, Req, S};
resource_exists(Req, #state{section=mcu, index=undefined}=S) ->
    {true, Req, S};
resource_exists(Req, #state{section=mcu, index=I}=S) ->
    case mnesia:dirty_match_object(#ampTable{index=I, _='_'}) of
		[] ->
			{false, Req, S};
		[Mcu] ->
			{true, Req, S#state{mcu=Mcu}}
    end;
resource_exists(Req, #state{section=sys, sys=Sys}=S) when Sys =:= login;
														  Sys =:= net;
														  Sys =:= reset;
														  Sys =:= reboot;
														  Sys =:= password;
														  Sys =:= community;
														  Sys =:= usm;
														  Sys =:= protocol;
														  Sys =:= targets;
														  Sys =:= usb ->
    {true, Req, S};
resource_exists(Req, #state{section=sys}=S) ->
    {false, Req, S};
resource_exists(Req, #state{section=firmware}=S) ->
    {true, Req, S};
resource_exists(Req, #state{section={firmware, _}}=S) ->
    {true, Req, S};
resource_exists(Req, #state{section=undefined}=S) ->
    {false, Req, S};
resource_exists(Req, S) ->
    {true, Req, S}.


allow_missing_post(Req, State) ->
    {false, Req, State}.


to_json(Req, #state{section=mcu, index=undefined, version=Version}=S) ->
    Mcus = mnesia:dirty_match_object(#ampTable{_='_'}),
    Ejson = lists:map(fun (Mcu) -> bkfw_mcu:get_kv(Mcu, Version) end, Mcus),
    {jsx:encode(Ejson, ?JSX_OPTS), Req, S};

to_json(Req, #state{section=mcu, mcu=Mcu, version=Version}=S) ->
    {jsx:encode(bkfw_mcu:get_kv(Mcu, Version), ?JSX_OPTS), Req, S};

to_json(Req, #state{section=edfa, version=Version}=S) ->
    {jsx:encode(bkfw_edfa:get_kv(Version), ?JSX_OPTS), Req, S};

to_json(Req, #state{section=sys, sys=Cat}=S) when Cat =:= login;
												  Cat =:= net;
												  Cat =:= community;
												  Cat =:= usm;
												  Cat =:= protocol;
												  Cat =:= targets;
												  Cat =:= usb->
    {jsx:encode(bkfw_config:get_kv(Cat)), Req, S};

to_json(Req, #state{section=firmware}=S) ->
    {jsx:encode(bkfw_config:get_kv(firmware)), Req, S};

to_json(Req, #state{section=params}=S) ->
	Json = [
			{'has_PC_mode', true},
			{'has_GC_mode', true},
			{'has_input_PD', true},
			{'has_output_PD', true},
			{'number_of_edfa', 1}
		   ],
    {jsx:encode(Json, ?JSX_OPTS), Req, S};

to_json(Req, #state{section=sys, sys=_}=S) ->
    {<<"{}">>, Req, S}.


from_json(Req, #state{section=mcu, index=undefined}=S) ->
    case parse_body(Req) of
		{error, invalid_body, Req2} ->
			{false, ?set_error(invalid_body, Req2), S};
		{error, Err, Req2} ->
			?error("Internal error: ~p~n", [Err]),
			{halt, ?set_error(internal, Req2), S};
		{ok, Json, Req2} ->
			?debug("POST /api/mcu/\n"
				   "       ~p\n", [Json]),
			Indices = mnesia:dirty_all_keys(ampTable),
			Err = lists:foldl(fun (I, Acc) ->
									  case bkfw_mcu:set_kv(I, Json) of
										  ok -> Acc;
										  {error, Err} ->
											  ?error("Request error: ~p~n", [Err]),
											  [ [io_lib:format("amp #~p: ", [I]), err_to_string(Err)] | Acc ]
									  end
							  end, [], Indices),
			case Err of
				[] -> {true, Req2, S};
				Errors -> {false, ?set_errors(Errors, Req2), S}
			end		
    end;
from_json(Req, #state{section=mcu, index=I}=S) ->
    case parse_body(Req) of
		{error, invalid_body, Req2} ->
			{false, ?set_error(invalid_body, Req2), S};
		{error, Err, Req2} ->
			?error("Internal error: ~p~n", [Err]),
			{halt, ?set_error(internal, Req2), S};
		{ok, Json, Req2} ->
			?debug("POST /api/mcu/~p\n"
				   "       ~p\n", [I, Json]),
			case bkfw_mcu:set_kv(I, Json) of
				ok ->
					?debug("Set MCU kv=ok\n"),
					{true, Req2, S};
				{error, Err} ->
					?error("Request error: ~p~n", [Err]),
					{false, ?set_error(Err, Req2), S}
			end
    end;
from_json(Req, #state{section=sys, sys=login}=S) ->
    case parse_body(Req) of
		{error, invalid_body, Req2} ->
			{false, ?set_error(invalid_body, Req2), S};
		{error, Err, Req2} ->
			{false, ?set_error(Err, Req2), S};
		{ok, Json, Req2} ->
			case auth_user(proplists:get_value(login, Json),
						   proplists:get_value(password, Json)) of
				true ->
					{true, Req2, S};
				false ->
					{false, ?set_error(invalid_login, Req2), S}
			end
    end;
from_json(Req, #state{section=sys, sys=Cat}=S) ->
    case parse_body(Req) of
		{error, invalid_body, Req2} ->
			{false, ?set_error(invalid_body, Req2), S};
		{error, Err, Req2} ->
			?error("Internal error: ~p~n", [Err]),
			{halt, ?set_error(internal, Req2), S};
		{ok, Json, Req2} ->
			case bkfw_config:set_kv(Cat, Json) of
				ok ->
					{true, Req2, S};
				{error, Err} ->
					?error("Request error: ~p~n", [Err]),
					{false, ?set_error(Err, Req2), S}
			end
    end.

from_multipart(Req, #state{section={firmware, Fw}, firmware=Path}=S) ->
    case cowboy_req:part(Req) of
		{ok, Hdr, Req2} ->
			case cow_multipart:form_data(Hdr) of
				{data, _Name} ->
					{ok, _Body, Req3} = cowboy_req:part_body(Req2),
					from_multipart(Req3, S);
				{file, _Field, _Filename, _ContentType, _Enc} ->
					Fullpath = filename:join(application:get_env(bkfw, upload_dir, ""), Fw ++ ".bin"),
					case stream_file(Fullpath, Req2) of
						{ok, Req3} ->
							from_multipart(Req3, S#state{firmware=Fullpath});
						{error, Err} ->
							?error("Error streaming file: ~p~n", [Err]),
							{halt, Req2, S}
					end
			end;
		{done, Req2} ->
			case bkfw_config:upgrade(Fw, Path) of
				ok ->
					{true, Req2, S};
				{error, _} = Err ->
					?error("Error upgrading firmware: ~p", [Err]),
					{false, Req2, S}
			end
    end.

parse_body(Req) ->
    case cowboy_req:body(Req) of
		{ok, <<>>, Req2} ->
			{error, invalid_body, Req2};
		{ok, Body, Req2} ->
			try jsx:decode(Body, [{labels, attempt_atom}]) of
				Props when is_list(Props) ->
					{ok, Props, Req2}
			catch error:badarg ->
					{error, invalid_body, Req2}
			end;
		{error, Err} ->
			{error, Err}
    end.

stream_file(Filename, Req) ->
    case file:open(Filename, [write]) of
		{ok, File} ->
			stream_file(File, Req, 0);
		{error, Err} ->
			{error, Err}
    end.

stream_file(File, _Req, Size) when Size > ?MAX_SIZE ->
    file:close(File),
    {error, max_size};

stream_file(File, Req, Size) ->
    case cowboy_req:part_body(Req) of
		{ok, Data, Req2} ->
			case file:write(File, Data) of
				ok ->
					file:close(File),
					{ok, Req2};
				{error, Err} ->
					?error("Error writing file: ~p~n", [Err]),
					{error, Err}
			end;
		{more, Data, Req2} ->
			case file:write(File, Data) of
				ok ->
					stream_file(File, Req2, Size + byte_size(Data));
				{error, Err} ->
					{error, Err}
			end
    end.

require_auth(Req, State) ->
    case cowboy_req:header(<<"authorization">>, Req) of
		{undefined, Req2} ->
			{{false, auth_header(Req, State)}, Req2, State};
		{Value, Req2} ->
			case parse_auth(Value) of
				{basic, Auth} ->
					auth_basic_user(Auth, Req, State);
				{digest, _Auth} ->
					%% Unsupported
					{{false, auth_header(Req, State)}, Req2, State};
				{error, Err} ->
					?error("Error authenticating: ~p~n", [Err]),
					{{false, auth_header(Req, State)}, Req2, State}
			end
    end.

auth_basic_user(Auth, Req, State) ->
    try binary:split(base64:decode(Auth), [<<":">>]) of
		[User, Password] ->
			case auth_user(User, Password) of
				true ->
					{true, Req, State};
				false ->
					{{false, auth_header(Req, State)}, Req, State}
			end;
		_ ->
			{{false, auth_header(Req, State)}, Req, State}
    catch error:_Err ->
			{{false, auth_header(Req, State)}, Req, State}
    end.


auth_header(_Req, _) ->
	[ "x-basic realm=\"", ?REALM, "\""].


auth_user(<<"admin">>, Password) ->
    case get_password() of
		undefined -> false;
		{md5, Hash} ->
			case base64:encode(Password) of
				Hash -> true;
				_ -> false
			end
    end;

auth_user(_, _) ->
    false.


get_password() ->
    case application:get_env(bkfw, password, undefined) of
		undefined -> undefined;
		{Method, Hash} when Method =:= md5;
							Method =:= sha ->
			{Method, list_to_binary(Hash)};
		_ -> undefined
    end.


-spec parse_auth(binary()) -> {basic | digest | error, term()}.
parse_auth(Bin) ->
    parse_method(parse_next(Bin)).


parse_method({Method, Rest}) ->
    case to_lower(Method) of
		<<"x-basic">> -> parse_basic_hash(Rest);
		<<"x-digest">> -> parse_digest(Rest);
		M -> {error, {invalid_method, M}}
    end.


parse_basic_hash(Bin) ->
    {basic, Bin}.

parse_digest(Bin) ->
    {digest, Bin}.

parse_next(Bin) ->
    case binary:split(Bin, <<" ">>, [trim]) of
		[<<>>, Rest] -> parse_next(Rest);
		[Next, Rest] -> {Next, Rest};
		[Next] -> {Next, <<>>}
    end.

to_lower(Bin) ->
    to_lower(Bin, <<>>).

to_lower(<<>>, Acc) ->
    Acc;
to_lower(<< C, Rest/bits >>, Acc) ->
    case C of
		$A -> to_lower(Rest, << Acc/binary, $a >>);
		$B -> to_lower(Rest, << Acc/binary, $b >>);
		$C -> to_lower(Rest, << Acc/binary, $c >>);
		$D -> to_lower(Rest, << Acc/binary, $d >>);
		$E -> to_lower(Rest, << Acc/binary, $e >>);
		$F -> to_lower(Rest, << Acc/binary, $f >>);
		$G -> to_lower(Rest, << Acc/binary, $g >>);
		$H -> to_lower(Rest, << Acc/binary, $h >>);
		$I -> to_lower(Rest, << Acc/binary, $i >>);
		$J -> to_lower(Rest, << Acc/binary, $j >>);
		$K -> to_lower(Rest, << Acc/binary, $k >>);
		$L -> to_lower(Rest, << Acc/binary, $l >>);
		$M -> to_lower(Rest, << Acc/binary, $m >>);
		$N -> to_lower(Rest, << Acc/binary, $n >>);
		$O -> to_lower(Rest, << Acc/binary, $o >>);
		$P -> to_lower(Rest, << Acc/binary, $p >>);
		$Q -> to_lower(Rest, << Acc/binary, $q >>);
		$R -> to_lower(Rest, << Acc/binary, $r >>);
		$S -> to_lower(Rest, << Acc/binary, $s >>);
		$T -> to_lower(Rest, << Acc/binary, $t >>);
		$U -> to_lower(Rest, << Acc/binary, $u >>);
		$V -> to_lower(Rest, << Acc/binary, $v >>);
		$W -> to_lower(Rest, << Acc/binary, $w >>);
		$X -> to_lower(Rest, << Acc/binary, $x >>);
		$Y -> to_lower(Rest, << Acc/binary, $y >>);
		$Z -> to_lower(Rest, << Acc/binary, $z >>);
		_ -> to_lower(Rest, << Acc/binary, C >>)
    end.

json_error(Errors) ->
    jsx:encode([ err_to_string(E) || E <- Errors ], ?JSX_OPTS).

err_to_string(internal) -> <<"Internal error in backend">>;
err_to_string(invalid_body) -> <<"JSON error">>;
err_to_string(invalid_login) -> <<"Invalid login and/or password">>;
err_to_string(missing_mode) -> <<"Missing value: operating mode">>;
err_to_string(missing_consign) -> <<"Missing value: consign">>;
err_to_string(missing_net_type) -> <<"Missing value: network type">>;
err_to_string(invalid_fw) -> <<"Invalid firmware">>;
err_to_string(invalid_net_config) -> <<"Invalid network configuration">>;
err_to_string(invalid_net_address) -> <<"Invalid value: network address">>;
err_to_string(invalid_net_mask) -> <<"Invalid value: network mask">>;
err_to_string(invalid_net_gw) -> <<"Invalid value: network gateway">>;
err_to_string(invalid_community) -> <<"Invalid value: community name">>;
err_to_string(invalid_target) -> <<"Invalid value: target address">>;
err_to_string(invalid_thresholds) -> <<"Invalid value: thresholds">>;
err_to_string(invalid_snmp_authkey) -> <<"Authentication key must be at least 8-chars long">>;
err_to_string(invalid_snmp_privkey) -> <<"Privacy key must be at least 8-chars long">>;
err_to_string(invalid_payload) -> <<"Error writing firmware">>;
err_to_string(ofr) -> <<"Consign out of range, check your manual">>;
err_to_string(empty_password) -> <<"Invalid value: empty password">>;
err_to_string({unexpected, _}) -> <<"Internal error in backend">>;
err_to_string(Else) when is_atom(Else) -> atom_to_binary(Else, utf8);
err_to_string(Else) when is_list(Else) -> list_to_binary(Else);
err_to_string(Else) when is_binary(Else) -> Else.

set_errors(Errors, Req) ->
    Req2 = cowboy_req:set_resp_body(json_error(Errors), Req),
    cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2).

