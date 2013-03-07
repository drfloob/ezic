-module(ezic).
-include("include/ezic.hrl").

-export([
	 localtime/1
	 , utc_to_local/2
	 , local_to_utc/2
	 , has_dst_utc/2
	 , has_dst_local/2
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% returns local time in given timezone
localtime(TzName) ->
    utc_to_local(erlang:universaltime(), TzName).

%% returns time in given timezone for corresponding utc time
utc_to_local(UTCDatetime, TzName) ->
    NormalDatetime= ezic_date:normalize(UTCDatetime, u),
    utc_to_local_handleFlatzone(UTCDatetime, ezic_db:flatzone(NormalDatetime, TzName)).



%% returns utc time for corresponding time in given timezone utc
local_to_utc(LocalDatetime, TzName) ->
    NormalDatetime= ezic_date:normalize(LocalDatetime, w),
    local_to_utc_handleFlatzone(LocalDatetime, ezic_db:flatzone(NormalDatetime, TzName)).


has_dst_utc(Datetime, TzName) ->
    has_dst(Datetime, TzName, u).

has_dst_local(Datetime, TzName) ->
    has_dst(Datetime, TzName, w).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

utc_to_local_handleFlatzone(_, X={error,_}) ->
    X;
utc_to_local_handleFlatzone(UTCDatetime, #flatzone{offset=Offset, dstoffset=DSTOffset}) ->
    ezic_date:add_offset(
      ezic_date:add_offset(
    	UTCDatetime
    	, Offset)
      , DSTOffset).


local_to_utc_handleFlatzone(_, X={error, _}) ->
    X;
local_to_utc_handleFlatzone(LocalDatetime, #flatzone{offset=Offset, dstoffset=DSTOffset}) ->
    ezic_date:add_offset(
      ezic_date:add_offset(
    	LocalDatetime
    	, Offset, {0,0,0})
      , DSTOffset, {0,0,0}).

has_dst(Datetime, TzName, Flag) ->
    NormalDatetime = ezic_date:normalize(Datetime, Flag),
    case ezic_db:flatzone(NormalDatetime, TzName) of
        #flatzone{dstoffset={0,0,0}} ->
            false;
        #flatzone{dstoffset={_,_,_}} ->
            true
    end.

