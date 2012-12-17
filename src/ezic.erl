-module(ezic).
-include("include/ezic.hrl").

-export([
	 localtime/1
	 , utc_to_local/2
	 , local_to_utc/2
	 , next_timechange/1
	 , next_timechange_after/2
	]).


-export([
	 load/1
	 , dev/0
	 , test/0
	 , reflatten/0
	 , zf/0
	]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



localtime(TzName) ->
    utc_to_local(erlang:universaltime(), TzName).


utc_to_local(UTCDatetime, TzName) ->
    NormalDatetime= ezic_date:normalize(UTCDatetime, u),
    utc_to_local_handleFlatzone(UTCDatetime, ezic_db:flatzone(NormalDatetime, TzName)).
    
    

local_to_utc(LocalDatetime, TzName) ->
    NormalDatetime= ezic_date:normalize(LocalDatetime, w),
    #flatzone{offset=Offset, dstoffset=DSTOffset}= ezic_db:flatzone(NormalDatetime, TzName),
    
    ezic_date:add_offset(
      ezic_date:add_offset(
	LocalDatetime
	, Offset, {0,0,0})
      , DSTOffset, {0,0,0}).
	    


next_timechange(TzName) ->
    next_timechange_after(erlang:universaltime(), TzName).

next_timechange_after(_UTCDatetime, _TzName) ->
    not_done.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEV API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



load(Folder) ->
    ezic_loader:load(Folder).


dev() ->
    application:start(ezic),
    ezic_db:flatzone({{2010,11,17}, #tztime{time={23,42,0}}}, "America/Los_Angeles"),
    ok.

zf() ->
    Zones= ezic_db:zones("Asia/Tokyo"),
    ezic_flatten:flatten_all_zones(Zones).
    

reflatten() ->
    ezic_db:wipe(flatzone),
    ezic_db:flatten().



test() ->
    lists:foreach(
      fun(TZ)->
	      io:format("~s: ~p~n", [TZ, localtime(TZ)])
      end,
      ["Asia/Tokyo", "America/New_York", "America/Los_Angeles", "America/Jamaica", "Australia/Adelaide"]).



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
