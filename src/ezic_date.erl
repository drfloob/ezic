-module(ezic_date).
-include("include/ezic.hrl").



-ifdef(TEST).
-compile([export_all]).
-endif.




-export([for/2]).
-export([month_to_num/1, day_to_num/1]).
-export([compare_times/2, compare_datetimes/2, normalize/1, overlap/2, date_between/2]).

-export([add_seconds/2, all_times/3]).





% returns the date tuple for a given rule and year
% same for all timezones.
% e.g. -> {Y,M,D}
for(#rule{in=M, on={last, D}}, Y)  ->
    last_day_of(D, Y,M);
for(#rule{in=M, on=#tzon{day=Day, filter=Filter}}, Y) ->
    first_day_limited(Day, Filter, Y,M).


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
previous_day(Day, Date={Y,M,D}) ->
    Daynum= day_to_num(Day),
    LeqDoW= calendar:day_of_the_week(Date),
    case Daynum=:=LeqDoW of
	true -> Date;
	_ -> 
	    DayDiff= case LeqDoW-Daynum < 0 of
			 true -> LeqDoW-Daynum+7;
			 _ -> LeqDoW-Daynum
		     end,
	    case D-DayDiff < 0 of
		true -> erlang:error(no_previous_day, [Day, Date]);
		false -> {Y,M,D-DayDiff}
	    end
    end.


% Returns the soonest date on which Day (sun/mon/tue/etc.) occurs AFTER the given date.
% same for all timezones
next_day(Day, Date={Y,M,D}) ->
    Daynum= day_to_num(Day),
    LeqDoW= calendar:day_of_the_week(Date),
    case Daynum=:=LeqDoW of
	true -> Date;
	_ -> 
	    DayDiff= case Daynum-LeqDoW < 0 of
			 true -> Daynum-LeqDoW+7;
			 _ -> Daynum-LeqDoW
		     end,
	    case D+DayDiff > calendar:last_day_of_the_month(Y,M) of
		true -> erlang:error(no_next_day, [Day, Date]);
		false -> {Y,M,D+DayDiff}
	    end
    end.



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
day_to_num(_) ->    -1.



compare_datetimes(DT1, DT2) ->
    NDT1= normalize(DT1),
    NDT2= normalize(DT2),
    compare_datetimes_normal(NDT1, NDT2).


compare_datetimes_normal(minimum, _) -> true;
compare_datetimes_normal(_, maximum) -> true;


compare_datetimes_normal(current, current) -> true;
compare_datetimes_normal(current, _) -> false;
compare_datetimes_normal(_,current) -> true;

% @bug this is not true if the times are from different time zones
% @todo check types for D1 and D2
compare_datetimes_normal({D1, _}, {D2, _}) when D1 < D2 ->
    true;
compare_datetimes_normal({D1, _}, {D2, _}) when D1 > D2 ->
    false;
compare_datetimes_normal({_, T1}, {_, T2}) ->
    compare_times(T1, T2).
		    

compare_times(TZ1=#tztime{}, TZ2=#tztime{}) ->
    {NT1, NT2} = time_equalize(TZ1, TZ2),
    NT1 =< NT2;
compare_times(Tz1, Tz2) ->
    erlang:error(wtf_not_done, [Tz1, Tz2]).



time_equalize(#tztime{time=T1, flag=F1}, #tztime{time=T2, flag=F2}) when F1=:=F2 ->
    {T1, T2};
time_equalize(T1, T2) ->
    %@todo this may be difficult. gotta do it!
    erlang:error(wtf_not_done, T1, T2).
    


normalize(current) ->  current;
normalize(only) ->  only;
normalize(maximum) ->  maximum;
normalize(minimum) ->  minimum;


normalize(Y) when is_integer(Y) ->
    {{Y,1,1}, #tztime{}};
normalize(Date={_,_,D}) when is_integer(D) ->
    {Date, #tztime{}};
normalize({Y,M,{last, D}}) ->
    Date=last_day_of(D, Y,M),
    {Date, #tztime{}};


normalize({Date={_,_,_},Time={H,_,_}}) when is_integer(H) ->
    {Date, #tztime{time=Time}};

normalize({Date={_,_,D}, Tz=#tztime{}}) when is_integer(D) ->
    {Date, Tz};
normalize({{Y,M,#tzon{day=Day, filter=Filter}}, Tz=#tztime{}}) ->
    Date= first_day_limited(Day, Filter, Y,M),
    {Date, Tz};
normalize({{Y,M,{last, D}}, Tz=#tztime{}}) ->
    Date=last_day_of(D, Y,M),
    {Date, Tz};
normalize(X) ->
    ?debug("normalize: ~p", [X]),
    erlang:error(wtf_not_done, X).
    



overlap({D1s,D1e}, {D2s,D2e}) ->
    D1sN= normalize(D1s),
    D1eN= normalize(D1e),
    D2sN= normalize(D2s),
    D2eN= normalize(D2e),
    dates_overlap_normal({D1sN,D1eN}, {D2sN,D2eN}).


dates_overlap_normal({D1s,D1e}, {D2s,D2e}) ->
    date_between_normal(D1s, {D2s,D2e}) 
	orelse date_between_normal(D1e, {D2s,D2e}).


date_between(D1, {D2s, D2e}) ->
    D1N= normalize(D1),
    D2sN= normalize(D2s),
    D2eN= normalize(D2e),
    date_between_normal(D1N, {D2sN, D2eN}).


date_between_normal(minimum, {minimum,_}) -> true;
date_between_normal(minimum, {_,_}) -> false;

date_between_normal(maximum, {_, maximum}) -> true;
date_between_normal(maximum, {_,_}) -> false;

date_between_normal(only, _) -> false;
date_between_normal(current, {_, current}) -> true;
date_between_normal(current, {_, End}) when End =/= current -> false;
date_between_normal(D1, {D2, only}) ->
    {{Y1,_,_},_}=D1,
    {{Y2,_,_},_}=D2,
    Y1 =:= Y2;
date_between_normal(D1, {D2s, D2e}) ->
    compare_datetimes_normal(D2s, D1) 
	andalso compare_datetimes_normal(D1, D2e).
    
    


add_seconds(Datetime, Seconds) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:date_to_gregorian_seconds(Datetime) + Seconds
     ).





% returns {WallTime, StdTime, UtcTime} 
% where each is a datetime tuple: {{Y,M,D}{HH,MM,SS}}

% universal time given
all_times(Datetime=#tztime{time=UTCTime, flag=Flag}, Offset, DSTOffset) 
  when Flag=:=u; Flag=:=g; Flag=:=z ->
    OSec= calendar:time_to_seconds(Offset),
    DSTSec= calendar:time_to_seconds(DSTOffset),

    STDTime= add_seconds(UTCTime, OSec),
    WallTime= add_seconds(STDTime, DSTSec),

    {WallTime, STDTime, UTCTime};


% standard time given
all_times(Datetime=#tztime{time=STDTime, flag=s}, Offset, DSTOffset) ->
    OSec= calendar:time_to_seconds(Offset),
    DSTSec= calendar:time_to_seconds(DSTOffset),

    UTCTime= add_seconds(STDTime, -1*OSec),
    WallTime= add_seconds(STDTime, DSTSec),

    {WallTime, STDTime, UTCTime};


% wall time given
all_times(Datetime=#tztime{time=WallTime, flag=Flag}, Offset, DSTOffset) 
  when Flag=:=w, Flag=:=undefined ->
    OSec= calendar:time_to_seconds(Offset),
    DSTSec= calendar:time_to_seconds(DSTOffset),

    STDTime= add_seconds(WallTime, -1*DSTSec),
    UTCTime= add_seconds(STDTime, -1*OSec),

    {WallTime, STDTime, UTCTime}.


