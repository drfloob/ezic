-module(ezic).
-include("include/ezic.hrl").

-export([localtime/1, time2time/3]).
-export([load/1, dev_start/0, test/0]).


-define(gs2dt(X), calendar:gregorian_seconds_to_datetime(X)).
-define(dt2gs(X), calendar:datetime_to_gregorian_seconds(X)).



localtime(TzName) ->
    time(erlang:universaltime(), TzName).


time(Datetime, TzName) ->
    
    Zones= ezic_db:zones(TzName),
%    ?debug("All Zones: ~p", [Zones]),
    CZone= ezic_zone:current(Datetime, Zones),
%    ?debug("Current Zone: ~p", [CZone]),

    RuleName= CZone#zone.rule,
    Rules= ezic_db:rules(RuleName),
%    ?debug("All Rules: ~p", [Rules]),
    CRule= ezic_rule:current(Datetime, Rules),
%    ?debug("Current Rule: ~p", [CRule]),

    OffsetSec= ezic_zone:offset_sec(CZone),
    DSTSec= ezic_rule:dst_sec(CRule),
    
    SecDiff= OffsetSec + DSTSec,
    ?gs2dt(?dt2gs(Datetime) + SecDiff).
    


time2time(_DateTime, _FromTimeZone, _ToTimeZone) ->
    void.






load(Folder) ->
    ezic_loader:load(Folder).


dev_start() ->
    ezic:load(filename:join("priv","tzdata")).


test() ->
    lists:foreach(
      fun(TZ)->
	      io:format("~s: ~p~n", [TZ, localtime(TZ)])
      end,
      ["Asia/Tokyo", "America/New_York", "America/Los_Angeles", "America/Jamaica", "Australia/Adelaide"]).
