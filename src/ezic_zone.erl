-module(ezic_zone).
-include("include/ezic.hrl").

-export([current/2, sort/2, revsort/2, offset_sec/1]).


current(_, []) ->
    erlang:error(no_current);
current(Now, Zones) ->
    SortedZones= lists:sort(fun revsort/2, Zones),
%    ?debug("SortedZones: ~p", [SortedZones]),
    [CZone|_]= lists:dropwhile(fun(SZ)-> older_zone(SZ, Now) end, SortedZones),
    CZone.
			       


sort(#zone{until=U1}, #zone{until=U2}) ->
    ezic_date:compare_datetimes(U2, U1).

revsort(Z1=#zone{}, Z2=#zone{}) ->
    not sort(Z1, Z2).

older_zone(Zone=#zone{until=Until}, Now) ->
    ezic_date:compare_datetimes(Until, Now).



offset_sec(Zone) ->
    calendar:time_to_seconds((Zone#zone.gmtoff)#tztime.time).
