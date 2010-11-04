-module(ezic_date).
-include("include/ezic.hrl").



-ifdef(TEST).
-compile([export_all]).
-endif.


-export([for/2]).
-export([month_to_num/1, day_to_num/1]).
-export([compare_times/2, compare_datetimes/2, normalize/1, overlap/2, date_between/2]).





% returns the concrete date tuple for a given rule and year, irrespective of zone
% e.g. -> {Y,M,D}
for(#rule{in=M, on={last, D}}, Y)  ->
    last_day_of(D, Y,M);
for(#rule{in=M, on=#tzon{day=Day, filter=Filter}}, Y) ->
    first_day_limited(Day, Filter, Y,M).



last_day_of(Day, Y,M) ->
    LastDay= calendar:last_day_of_the_month(Y,M),
    previous_day(Day, {Y,M,LastDay}).


% returns a date tuple {Y,M,D} representing the first available date, given the filter
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



compare_datetimes(DT1, DT2) ->
    NDT1= normalize(DT1),
    NDT2= normalize(DT2),
    compare_datetimes_normal(NDT1, NDT2).


compare_datetimes_normal(minimum, _) -> true;
compare_datetimes_normal(_, maximum) -> true;


compare_datetimes_normal(current, current) -> true;
compare_datetimes_normal(current, _) -> false;
compare_datetimes_normal(_,current) -> true;

compare_datetimes_normal({D1, T1}, {D2, T2}) when D1 < D2 ->
    true;
compare_datetimes_normal({D1, T1}, {D2, T2}) when D1 > D2 ->
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


normalize({Date={Y,M,D},Time={H,_,_}}) when is_integer(H) ->
    {Date, #tztime{time=Time}};

normalize({Date={Y,M,D}, Tz=#tztime{}}) when is_integer(D) ->
    {Date, Tz};
normalize({{Y,M,TzOn=#tzon{day=Day, filter=Filter}}, Tz=#tztime{}}) ->
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
    
    
