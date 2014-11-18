-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(Msg), io:format("DEBUG: " ++ Msg)).
-define(debug(Msg, Data), io:format("DEBUG: " ++ Msg, Data)).
-else.
-define(debug(Msg), true).
-define(debug(Msg, Data), true).
-endif.

-define(info(Msg), io:format("INFO: " ++ Msg)).
-define(info(Msg, Data), io:format("INFO: " ++ Msg, Data)).

-define(error(Msg), io:format("ERROR: " ++ Msg)).
-define(error(Msg, Data), io:format("ERROR: " ++ Msg, Data)).
