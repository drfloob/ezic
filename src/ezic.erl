-module(ezic).
-include("include/ezic.hrl").

-export([
	 localtime/1
	 , utc_to_local/2
%	 , local_to_utc/2
%	 , zone_convert/3
	 , next_timechange/1
	 , next_timechange/2
	]).

-export([load/1, dev_start/0, test/0]).


-define(gs2dt(X), calendar:gregorian_seconds_to_datetime(X)).
-define(dt2gs(X), calendar:datetime_to_gregorian_seconds(X)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



localtime(TzName) ->
    utc_to_local(erlang:universaltime(), TzName).


utc_to_local(UTCDatetime, TzName) ->
    {ok, Zone, Rule}= current_as_of_utc(UTCDatetime, TzName),
    SecDiff= time_offset(Zone, Rule),
    date_add(UTCDatetime, SecDiff).

    
%% local_to_utc(Datetime, TzName) ->
%%     {ok, Zone, Rule}= current_as_of_local(Datetime, TzName),
%%     SecDiff= time_offset(Zone, Rule),
%%     date_subtract(Datetime, SecDiff).
    

%% zone_convert(Datetime, FromTimeZone, ToTimeZone) ->
%%     UTC= local_to_utc(Datetime, FromTimeZone),
%%     utc_to_local(UTC, ToTimeZone).



next_timechange(TzName) ->
    next_timechange(erlang:universaltime(), TzName).

next_timechange(UtcDatetime, TzName) ->
    not_done.





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


current_as_of_utc(UTCDatetime, TzName) ->
    CZone= ezic_zone:current_as_of_utc(UTCDatetime, TzName),
    RuleName= CZone#zone.rule,
    Rules= ezic_db:rules(RuleName),
    CRule= ezic_rule:current_as_of_utc(UTCDatetime, Rules),
    {ok, CZone, CRule}.
    

%%current_as_of_local(Datetime, TzName) ->
%%    not_done.


time_offset(Zone, Rule) ->
    OffsetSec= ezic_zone:offset_sec(Zone),
    DSTSec= ezic_rule:dst_sec(Rule),
    OffsetSec + DSTSec.
    


date_add(Datetime, SecDiff) ->
    ?gs2dt(?dt2gs(Datetime) + SecDiff).    

date_subtract(Datetime, SecDiff) ->
    ?gs2dt(?dt2gs(Datetime) - SecDiff).
    
