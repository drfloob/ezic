
-record(rule, {name, from, to, type, in, on, at, save, letters}).


-record(zone, {name, gmtoff, rules, format, until}).


-record(link, {from, to}).


-record(leap, {datetime, corr, rs}).


-record(tztime, {time, flag}).
-record(tzon, {day, filter}).



% a flattened zone record, giving specific information for a specific time range.
-record(flatzone, {tzname, tzrule, from, to, gmtoff}).




-ifdef(debug).
-define(debug(F,X), io:format("{~p:~p} - " ++ F ++ "~n", [?MODULE, ?LINE] ++ X)).
-else.
-define(debug(F,X), void).
-endif.
