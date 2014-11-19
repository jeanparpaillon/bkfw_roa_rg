-module(bkfw_edfa_funcs).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

%% API
-export([
	 edfaNumber/1,
	 edfaTable/2,
	 edfaTable/4
	]).

%%----------------------------------------------------------------
%% Instrumentation function for variable myName.
%% Returns: (get) {value, Name}
%%          (set) noError
%%----------------------------------------------------------------
edfaNumber(new) ->
    ?info("Loading edfaNumber~n", []),
    ok;
edfaNumber(delete) ->
    ?info("Unloading edfaNumber~n", []),
    ok;
edfaNumber(get) ->
    ?info("get edfaNumber~n", []),
    {value, 5};
edfaNumber(_Op) ->
    ?info("~p edfaNumber~n", [_Op]),
    noError.

%%----------------------------------------------------------------
%% Instrumentation function for table edfaTable.
%%----------------------------------------------------------------
edfaTable(new, NameDb) ->
    snmp_generic:table_func(new, NameDb);
edfaTable(delete, NameDb) ->
    snmp_generic:table_func(delete, NameDb).


edfaTable(is_set_ok, RowIndex, Cols, NameDb) ->
    snmp_generic:table_func(is_set_ok, RowIndex, Cols, NameDb);

edfaTable(set, RowIndex, Cols, NameDb) ->
    snmp_generic:table_func(set, RowIndex, Cols, NameDb);

edfaTable(Op, RowIndex, Cols, NameDb) ->
    snmp_generic:table_func(Op, RowIndex, Cols, NameDb).
