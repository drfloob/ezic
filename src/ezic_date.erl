-module(ezic_date).
-include("include/ezic.hrl").



-ifdef(TEST).
-compile([export_all]).
-endif.


-export([for/2]).
-export([month_to_num/1, day_to_num/1]).
-export([compare_times/2, compare_dates/2, compare_datetimes/2]).





% returns the concrete date tuple for a given rule and year
for(#rule{in=M, on={last, D}}, Y)  ->
    last_day_of(D, Y,M);
for(#rule{in=M, on=#tzon{day=Day, filter=Filter}}, Y) ->
    first_day_limited(Day, Filter, Y,M).



last_day_of(Day, Y,M) ->
    LastDay= calendar:last_day_of_the_month(Y,M),
    previous_day(Day, {Y,M,LastDay}).


first_day_limited(Day, {geq, N}, Y,M) ->
    next_day(Day, {Y,M,N});
first_day_limited(Day, {leq, N}, Y,M) ->
    previous_day(Day, {Y,M,N}).
    



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



% returns true if A =< B, false otherwise
compare_datetimes(_, current) ->
    true;
compare_datetimes(current,_) ->
    false;

compare_datetimes(D1={_,_,_}, D2={_,_,_}) ->
    compare_dates(D1, D2);

compare_datetimes(D1={_,_,_}, DT2={_, #tztime{}}) ->
    compare_datetimes({D1, #tztime{}}, DT2);

compare_datetimes(DT1={_, #tztime{}}, D2={_,_,_}) ->
    compare_datetimes(DT1, {D2, #tztime{}});

compare_datetimes({D1, T1}, {D2, T2}) ->
    case compare_dates(D1, D2) of
	false -> % D1 > D2
	    false;
	true -> % D1 =< D2
	    case D1 =:= D2 of
		false -> % D1 < D2
		    true; 
		true -> % D1 = D2
		    compare_times(T1, T2)
	    end
    end.
		    

compare_dates(D1={_,_,_}, D2={_,_,_}) ->
    D1 =< D2.


compare_times(TZ1=#tztime{}, TZ2=#tztime{}) ->
    {NT1, NT2} = time_normalize(TZ1, TZ2),
    NT1 =< NT2;
compare_times(T1, T2) ->
    T1 =< T2.



time_normalize(#tztime{time=T1, flag=F1}, #tztime{time=T2, flag=F2}) when F1=:=F2 ->
    {T1, T2};
time_normalize(T1, T2) ->
    %@todo this may be difficult. gotta do it!
    erlang:error(wtf_not_done, T1, T2).
    

