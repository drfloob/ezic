-module(ezic_zone).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
	 parse/1
	 , current/1
	 , current_as_of_utc/2
	 , split_by_name/2
	 , sort_ascending/2
	 , sort_descending/2
	 , offset_sec/1

	 , project_end/2
	]).




parse([Name,GmtOffS,Rule,FormatS | UntilTokens]) ->
%    ?debugVal(GmtOffS),
    GmtOff= ezic_parse:time_val(GmtOffS),
    Until = ezic_parse:until(UntilTokens),
    Format= ezic_parse:tz_abbr(FormatS),
    
    {ok, #zone{
       name=Name, gmtoff=GmtOff, rule=Rule,
       format=Format, until=Until
      }}.










current(TzName) ->
    current_as_of_utc(erlang:universaltime(), TzName).

current_as_of_utc(UTCDatetime, TzName) ->
    Zones= ezic_db:zones(TzName),
    get_zone_utc(UTCDatetime, Zones).




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
    ezic_date:compare(U2, U1).

sort_ascending(Z1=#zone{}, Z2=#zone{}) ->
    not sort_descending(Z1, Z2).



% @bug doesn't normalize times. Times are self-relative (to standard time), and Now may come from any timezone unless we're careful about that.
%% older_zone(#zone{until=Until}, UTCDatetime) ->
%%     ezic_date:compare(Until, UTCDatetime).



offset_sec(Zone) ->
    calendar:time_to_seconds((Zone#zone.gmtoff)#tztime.time).




% returns the UTC datetime projected for zone end (given current DST Offset)
project_end(#zone{until=Until, gmtoff=Offset}, DSTOffset) ->
    {_,_,UTCDatetime}= ezic_date:all_times(Until, Offset, DSTOffset),
    UTCDatetime.




