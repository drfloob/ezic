-module(ezic_rule).
-include("include/ezic.hrl").


-export([parse/1]).
-export([next_event/4]).



parse([Name,FromS,ToS,_Type,InS,OnS,AtS,SaveS,Letters]) ->
    From= ezic_parse:year(FromS),
    To= ezic_parse:year(ToS),

    % @todo handle year types

    In= ezic_date:month_to_num(InS),
    On= ezic_parse:day_pattern(OnS),
    At= ezic_parse:time(AtS),
    
    Save= ezic_parse:time_val(SaveS),

    Rule= #rule{name=Name, from=From, to=To, type=not_done, 
		in=In, on=On, at=At, save=Save, letters=Letters},
    {ok, Rule}.



% determines the next zone or dst change event
% returns the latest possible times the previous period is in effect (`Until -1sec` in most cases)
% Rules are sorted
next_event(_BaseFlat, ZoneEnd, Offset, []) ->
    {end_until, make_times(ZoneEnd, Offset)};

% BaseFlat has offset defined
% @todo ensure offset defined on BaseFlat
% Rules are sorted in ascending order (oldest first)
next_event(_BaseFlat=#flatzone{utc_from=#tztime{time={{_Year,_,_},_}}}, 
	   _ZoneEnd, _ZoneOffset, [_Rule|_RestRules]) ->

    not_done.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% create endtimes for a #flatzone, with no dst offset
make_times(Until, Offset) ->
    make_times(Until, Offset, 0).

make_times(Until, Offset, DSTOffset) ->
    ezic_date:all_times(Until, Offset, DSTOffset).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




