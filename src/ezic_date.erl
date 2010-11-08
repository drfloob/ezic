-module(ezic_date).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").



-export([
	 % rule-specific
	 for_rule/2
	 , for_rule_zone/3


	 % converters
	 , month_to_num/1
	 , day_to_num/1


	 % date math
	 , add_seconds/2
	 , all_times/3
	]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% returns {Y,M,D} for a rule and Year
for_rule(#rule{in=M, on={last, D}}, Y) ->
    last_day_of(D, Y,M);
for_rule(#rule{in=M, on=#tzon{day=Day, filter=Filter}}, Y) ->
    first_day_limited(Day, Filter, Y,M).




% for_rule(#rule{}, Zone, Year) -> {WallTime, STDTime, UTCTime}
% returns the date set for a given rule and year
% same for all timezones.
for_rule_zone(#rule{in=M, on={last, D}, at=_Time, save=_DSTOffset}, _Zone=#zone{gmtoff=_Offset}, Y) ->
    _LDO= last_day_of(D, Y,M),
    
    not_done;
for_rule_zone(#rule{in=M, on=#tzon{day=Day, filter=Filter}}, _Zone, Y) ->
    first_day_limited(Day, Filter, Y,M),

    not_done.



month_to_num("Jan") -> 1;
month_to_num("Feb") -> 2;
month_to_num("Mar") -> 3;
month_to_num("Apr") -> 4;
month_to_num("May") -> 5;
month_to_num("Jun") -> 6;
month_to_num("Jul") -> 7;
month_to_num("Aug") -> 8;
month_to_num("Sep") -> 9;
month_to_num("Oct") -> 10;
month_to_num("Nov") -> 11;
month_to_num("Dec") -> 12;
month_to_num(X) -> erlang:error(badMonth, X).



day_to_num("Mon") -> 1;
day_to_num("Tue") -> 2;
day_to_num("Wed") -> 3;
day_to_num("Thu") -> 4;
day_to_num("Fri") -> 5;
day_to_num("Sat") -> 6;
day_to_num("Sun") -> 7;
day_to_num(X) ->    erlang:error(badday, X).



add_seconds(Datetime, Seconds) ->
    try
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(Datetime) + Seconds
     )
    catch
	error:Reason -> erlang:error(baddate, Datetime)
    end.





% returns {WallTime, StdTime, UtcTime} 
% where each is a datetime tuple: {{Y,M,D}{HH,MM,SS}}

% universal time given
all_times(#tztime{time=UTCTime, flag=Flag}, Offset, DSTOffset) 
  when Flag=:=u; Flag=:=g; Flag=:=z ->
    OSec= calendar:time_to_seconds(Offset),
    DSTSec= calendar:time_to_seconds(DSTOffset),

    STDTime= add_seconds(UTCTime, OSec),
    WallTime= add_seconds(STDTime, DSTSec),

    {WallTime, STDTime, UTCTime};


% standard time given
all_times(#tztime{time=STDTime, flag=s}, Offset, DSTOffset) ->
    OSec= calendar:time_to_seconds(Offset),
    DSTSec= calendar:time_to_seconds(DSTOffset),

    UTCTime= add_seconds(STDTime, -1*OSec),
    WallTime= add_seconds(STDTime, DSTSec),

    {WallTime, STDTime, UTCTime};


% wall time given
all_times(#tztime{time=WallTime, flag=Flag}, Offset, DSTOffset) 
  when Flag=:=w, Flag=:=undefined ->
    OSec= calendar:time_to_seconds(Offset),
    DSTSec= calendar:time_to_seconds(DSTOffset),

    STDTime= add_seconds(WallTime, -1*DSTSec),
    UTCTime= add_seconds(STDTime, -1*OSec),

    {WallTime, STDTime, UTCTime}.







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% last_day_of(Day, Y,M) -> {Y,M,D}
% returns the date on which the last Day (sun,mon,tue,etc.) occurs in a given month/year
% same for all timezones
last_day_of(Day, Y,M) ->
    LastDay= calendar:last_day_of_the_month(Y,M),
    previous_day(Day, {Y,M,LastDay}).


% returns a date tuple {Y,M,D} representing the first available date, given the filter
% same for all timezones
first_day_limited(Day, {geq, N}, Y,M) ->
    next_day(Day, {Y,M,N});
first_day_limited(Day, {leq, N}, Y,M) ->
    previous_day(Day, {Y,M,N}).
    


% Returns the soonest date on which Day (sun/mon/tue/etc.) occurs BEFORE the given date.
% same for all timezones
previous_day(Day, Date) ->
    Daynum= day_to_num(Day),
    LeqDoW= calendar:day_of_the_week(Date),
    case Daynum=:=LeqDoW of
	true -> Date;
	_ -> 
	    DayDiff= day_diff(LeqDoW, Daynum),
	    add_days_in_month(-DayDiff, Date)
    end.


% Returns the soonest date on which Day (sun/mon/tue/etc.) occurs AFTER the given date.
% same for all timezones
next_day(Day, Date) ->
    Daynum= day_to_num(Day),
    LeqDoW= calendar:day_of_the_week(Date),
    case Daynum=:=LeqDoW of
	true -> Date;
	_ -> 
	    DayDiff= day_diff(Daynum, LeqDoW),
	    add_days_in_month(DayDiff, Date)
    end.


% returns absolute number of days until the next day-of-the-week,
%   given some current day-of-the-week.
day_diff(From, To) ->
    case From - To < 0 of
	true -> From - To + 7;
	_ -> From - To
    end.
	     

% adds/subtracts days within a month
% errors-out if arguments require change of month
add_days_in_month(Days, Date={Y,M,D}) ->
    case D+Days < 1 of
	true -> erlang:error(no_previous_day, {Days, Date});
	_ -> 
	    case D+Days > calendar:last_day_of_the_month(Y,M) of
		true -> erlang:error(no_next_day, {Days, Date});
		_ -> {Y,M,D+Days}
	    end
    end.
		    
	    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



normalize(_Datetime) ->
    not_done.

compare_normal(_DT1, _DT2) ->
    not_done.

