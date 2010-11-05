-module(ezic).
-include("include/ezic.hrl").

-export([localtime/1, utc_to_local/2, utc_from_local/2, zone_convert/3]).

-export([load/1, dev_start/0, test/0]).


-define(gs2dt(X), calendar:gregorian_seconds_to_datetime(X)).
-define(dt2gs(X), calendar:datetime_to_gregorian_seconds(X)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



localtime(TzName) ->
    utc_to_local(erlang:universaltime(), TzName).


utc_to_local(Datetime, TzName) ->
    {ok, Zone, Rule}= current(Datetime, TzName),
    SecDiff= offset(Zone, Rule),
    date_add(Datetime, SecDiff).

    
utc_from_local(Datetime, TzName) ->
    {ok, Zone, Rule}= current(Datetime, TzName),
    SecDiff= offset(Zone, Rule),
    date_subtract(Datetime, SecDiff).
    

zone_convert(Datetime, FromTimeZone, ToTimeZone) ->
    UTC= utc_from_local(Datetime, FromTimeZone),
    utc_to_local(UTC, ToTimeZone).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEV API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


current(Datetime, TzName) ->
    Zones= ezic_db:zones(TzName),
%    ?debug("All Zones: ~p", [Zones]),
    CZone= ezic_zone:current(Datetime, Zones),
%    ?debug("Current Zone: ~p", [CZone]),

    RuleName= CZone#zone.rule,
    Rules= ezic_db:rules(RuleName),
%    ?debug("All Rules: ~p", [Rules]),
    CRule= ezic_rule:current(Datetime, Rules),
%    ?debug("Current Rule: ~p", [CRule]),
    {ok, CZone, CRule}.
    



offset(Zone, Rule) ->
    OffsetSec= ezic_zone:offset_sec(Zone),
    DSTSec= ezic_rule:dst_sec(Rule),
    OffsetSec + DSTSec.
    


date_add(Datetime, SecDiff) ->
    ?gs2dt(?dt2gs(Datetime) + SecDiff).    

date_subtract(Datetime, SecDiff) ->
    ?gs2dt(?dt2gs(Datetime) - SecDiff).
    
