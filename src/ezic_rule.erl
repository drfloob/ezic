-module(ezic_rule).
-include("include/ezic.hrl").


-export([next_event/4]).




% determines the next zone or dst change event
% returns the latest possible times the previous period is in effect (`Until -1sec` in most cases)
% Rules are sorted
next_event(BaseFlat, ZoneEnd, Offset, []) ->
    {end_until, make_times(ZoneEnd, Offset)};

% BaseFlat has offset defined
% @todo ensure offset defined on BaseFlat
% Rules are sorted in ascending order (oldest first)
next_event(BaseFlat=#flatzone{utc_from=#tztime{time={{Year,_,_},_}}}, 
	   ZoneEnd, ZoneOffset, [Rule|RestRules]) ->

    not_done.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% create endtimes for a #flatzone, with no dst offset
make_times(Until, Offset) ->
    make_times(Until, Offset, 0).

make_times(Until, Offset, DSTOffset) ->
    ezic_date:all_times(Until, Offset, DSTOffset).



