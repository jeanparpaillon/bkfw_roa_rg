-module(bkfw_edfa_funcs).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

%% API
-export([
	 start/0, 
	 edfaNumber/1,
	 edfaTable/1,
	 edfaTable/3
	]).

%% For internal use
-export([init/0]).

-define(SRV, ?MODULE).
-define(ENTRY, [
		32,     % amp consign
		2,     % gain consign
		43,     % output power consign
		2,     % operating mode
		86,     % laser temp
		3,     % amp
		56,     % internal temp
		"BKtel",     % vendor
		"laser",     % module type
		"hw3.04",     % hw ver
		"rev1",     % hw rev
		"sw0.1",     % sw ver
		"fw0.2",     % fw ver
		"42",     % part num
		"20141021",     % serial num
		"2014-10-15,12:34:00",      % product date
		1                 % row status
	       ]).
-define(TABLE, [
		list_to_tuple([ 1 | ?ENTRY ]),
		list_to_tuple([ 2 | ?ENTRY ]),
		list_to_tuple([ 3 | ?ENTRY ]),
		list_to_tuple([ 4 | ?ENTRY ]),
		list_to_tuple([ 5 | ?ENTRY ]),
		list_to_tuple([ 6 | ?ENTRY ])
	       ]).
-define(ENTRY_LEN, 18).

start() ->
    ok.

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
edfaTable(new) ->
    ?info("Loading edfaTable~n", []),
    ok;
edfaTable(delete) ->
    ?info("Unloading edfaTable~n", []),
    ok.

edfaTable(get, RowIndex, Cols) ->
    ?info("get edfaTable~n", []),
    case get_row(RowIndex) of
	{ok, Row} ->
	    get_cols(Cols, Row);
	_ ->
	    {noValue, noSuchInstance}
    end;

edfaTable(get_next, RowIndex, Cols) ->
    ?info("get_next edfaTable~n", []),
    case get_next_row(RowIndex) of
	{ok, Row} ->
	    get_next_cols(Cols, Row);
	_  ->
	    case get_next_row([]) of
		{ok, Row} ->
		    % Get next cols from first row.
		    NewCols = add_one_to_cols(Cols),
		    get_next_cols(NewCols, Row);
		_  ->
		    end_of_table(Cols)
	    end
    end.


%%%
%%% Priv
%%%
init() ->
    register(?SRV, self()),
    loop().

loop() ->
    receive
	_ ->
	    loop()
    end.

get_row(RowIndex) ->
    table_get_row(?TABLE, RowIndex).

get_next_row(RowIndex) ->
    table_get_next_row(?TABLE, RowIndex).

%%----------------------------------------------------------------
%% Make a list of endOfTable with as many elems as Cols list.
%%----------------------------------------------------------------
end_of_table([_Col | Cols]) ->
    [endOfTable | end_of_table(Cols)];
end_of_table([]) ->
    [].
add_one_to_cols([Col | Cols]) ->
    [Col + 1 | add_one_to_cols(Cols)];
add_one_to_cols([]) ->
    [].


%%----------------------------------------------------------------
%% Make a list of {value, Val} of the Row and Cols list.
%%----------------------------------------------------------------
get_cols([Col | Cols], Row) ->
    [{value, element(Col, Row)} | get_cols(Cols, Row)];
get_cols([], _Row) ->
    [].

%%----------------------------------------------------------------
%% As get_cols, but the Cols list may contain invalid column
%% numbers. If it does, we must find the next valid column,
%% or return endOfTable.
%%----------------------------------------------------------------
get_next_cols([Col | Cols], Row) when Col < 2 ->
    [{[2, element(1, Row)], element(2, Row)} | 
     get_next_cols(Cols, Row)];
get_next_cols([Col | Cols], Row) when Col > ?ENTRY_LEN ->
    [endOfTable | 
     get_next_cols(Cols, Row)];
get_next_cols([Col | Cols], Row) ->
    [{[Col, element(1, Row)], element(Col, Row)} | 
     get_next_cols(Cols, Row)];
get_next_cols([], _Row) ->
    [].

table_get_row([{Index, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}=Row | _], [Index]) ->
    {ok, Row};
table_get_row([_H | T], RowIndex) ->
    table_get_row(T, RowIndex);
table_get_row([], _RowIndex) ->
    no_such_row.

table_get_next_row([Row | _T], []) ->
    {ok, Row};
table_get_next_row([Row | _T], [Index | _]) when element(1, Row) > Index ->
    {ok, Row};
table_get_next_row([_Row | T], RowIndex) ->
    table_get_next_row(T, RowIndex);
table_get_next_row([], _RowIndex) ->
    endOfTable.
