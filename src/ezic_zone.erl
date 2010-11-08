-module(ezic_zone).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
	 current/1
	 , current_as_of_utc/2
	 , split_by_name/2
	 , sort_ascending/2
	 , sort_descending/2
	 , offset_sec/1
	]).




current(TzName) ->
    current_as_of_utc(erlang:universaltime(), TzName).

current_as_of_utc(UTCDatetime, TzName) ->
    Zones= ezic_db:zones(TzName),
    get_zone_utc(UTCDatetime, Zones).

%%current_as_of_local(_Datetime, _TzName) ->
%%    not_done.





get_zone_utc(_, []) ->
    erlang:error(no_current);
get_zone_utc(_UTCDatetime, Zones) ->
    _SortedZones= lists:sort(fun sort_descending/2, Zones),
%%    [CZone|_]= lists:dropwhile(fun(SZ)-> older_zone_utc(SZ, UTCDatetime) end, SortedZones),
%%    CZone.


    %% begin with GMT offset.
    %% foreach rule (oldest first)
    %%   see whether until=(standard|wall|utc) time
    %%   

    not_done.
			       



% returns {[SimilarZone], [DifferentZone]} 
%   where Similar Zones are those with the same name as Zone
%   and DifferentZones are all the rest
split_by_name(#zone{name=N}, Zones) ->
    lists:partition(
      fun(#zone{name=NI}) ->
	      N =:= NI
      end
      , Zones).


sort_descending(#zone{until=U1}, #zone{until=U2}) ->
    ezic_date:compare_datetimes(U2, U1).

sort_ascending(Z1=#zone{}, Z2=#zone{}) ->
    not sort_descending(Z1, Z2).



% @bug doesn't normalize times. Times are self-relative (to standard time), and Now may come from any timezone unless we're careful about that.
%% older_zone(#zone{until=Until}, UTCDatetime) ->
%%     ezic_date:compare_datetimes(Until, UTCDatetime).



offset_sec(Zone) ->
    calendar:time_to_seconds((Zone#zone.gmtoff)#tztime.time).
