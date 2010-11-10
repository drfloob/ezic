
-record(rule, {name, from, to, type, in, on, at, save, letters}).


-record(zone, {name, gmtoff, rule, format, until}).


-record(link, {from, to}).


-record(leap, {datetime, corr, rs}).


-record(tztime, {time={0,0,0}, flag=w}).
-record(tzon, {day, filter}).



% a flattened zone record, giving specific information for a specific time range.
-record(flatzone, {tzname, wall_from, wall_to, std_from, std_to, utc_from, utc_to, offset, dstoffset={0,0,0}}).




-ifdef(debug).
-define(debug(F,X), io:format("{~p:~p} - " ++ F ++ "~n", [?MODULE, ?LINE] ++ X)).
-define(debug(F), io:format("{~p:~p} - ~p~n", [?MODULE, ?LINE, F])).
-else.
-define(debug(F,X), void).
-define(debug(F), void).
-endif.




