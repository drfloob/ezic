-module(ezic).
-include("include/ezic.hrl").

-export([
	 localtime/1
	 , utc_to_local/2
	 , local_to_utc/2
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
    #flatzone{offset=Offset, dstoffset=DSTOffset}= ezic_db:flatzone(NormalDatetime, TzName),
    
    ezic_date:add_offset(
      ezic_date:add_offset(
	LocalDatetime
	, Offset, {0,0,0})
      , DSTOffset, {0,0,0}).



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
