-module(bkfw_edfa_funcs).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

%% API
-export([
	 edfaTable/2,
	 edfaTable/4
	]).

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
