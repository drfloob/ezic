-module(ezic_date).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").



-export([
	 normalize/1
	 , normalize/2

	 % rule-specific
	 , for_rule_relative/2
	 , for_rule/5
	 , for_rule_utc/4


	 % converters
	 , month_to_num/1
	 , day_to_num/1


	 % date math
	 , add_seconds/2
	 , add_offset/2
	 , add_offset/3
	 , all_times/3
	 , m1s/1
	 , m1s/3

	 % comparisons
	 , compare/2
	 , equal/2
	]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% converts any sane date part into a complete datetime
%% USED EXCLUSIVELY FOR COMPARISONS. The resulting "datetime" values may not
%%  represent true dates or times.
%% @todo ensure atom is valid
%% assumes wall time, if no #tztime{} given
normalize(A) when is_atom(A) ->
    A;
normalize(Y) when is_integer(Y) ->
    {{Y,1,1}, #tztime{}};
normalize({Y,M,D}) ->
    {relative_date(Y,M,D), #tztime{}};
normalize({{Y,M,D}, T=#tztime{}}) ->
    {relative_date(Y,M,D), T};
normalize({{Y,M,D}, T={_,_,_}}) ->
    {relative_date(Y,M,D), #tztime{time=T}}.


%% normalizes a date, and sets the #tztime{flag=Flag} if appropriate
%% @todo ensure flag is valid
%% @todo cover all the cases. this is currently just used for standard erlang datetimes
normalize({D={_,_,_},T={HH,_,_}}, Flag)
  when is_atom(Flag), is_integer(HH) ->

    DTz= {D, #tztime{time=T, flag=Flag}},
    normalize(DTz);

normalize(D, Flag) ->
    case normalize(D) of
	X when is_atom(X) -> X;
	X when is_record(X, tztime) ->
	    X#tztime{flag=Flag};
	_ ->
	    erlang:error(badDateTime, D)
	end.





%% returns RELATIVE datetime for a rule and Year
%%   -> {{Y,M,D},#tztime{}} | {{Y,M,D},{HH,MM,SS}}
for_rule_relative(#rule{in=M, on=D, at=At}, Y) ->
    {relative_date(Y,M,D), At}.



relative_date(Y,M,D) when is_integer(D) ->
    {Y,M,D};
relative_date(Y,M,{last, D}) ->
    last_day_of(D, Y,M);
relative_date(Y,M,#tzon{day=Day, filter=Filter}) ->
    first_day_limited(Day, Filter, Y,M).


%% returns set of ALL datetimes for a rule, given the gmt offset and
%% current dst offset.
%% @bug @todo Year is ambiguous. In the case of Africa/Tripoli, a jan 1st, 1952 rule shows up as 1951 due to offset and dst
for_rule(Rule, Offset, PrevDSTOffset, NextDSTOffset, Year) ->
    {WT,ST,UT}= for_rule_old_dst(Rule, Offset, PrevDSTOffset, Year),
    WTNew= add_offset(WT, PrevDSTOffset, NextDSTOffset),
    {{WT,WTNew}, ST, UT}.





for_rule_old_dst(Rule, Offset, PrevDSTOffset, Year) ->
    DT= for_rule_relative(Rule, Year),
    all_times(DT, Offset, PrevDSTOffset).



% returns UTC datetime for rule, offset, dst offset, and year
for_rule_utc(Rule, Offset, PrevDSTOffset, Year) ->
    {_,_,UTCDatetime} = for_rule_old_dst(Rule, Offset, PrevDSTOffset, Year),
    UTCDatetime.



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
	error:_ -> erlang:error(baddate, Datetime)
    end.




add_offset(Datetime, Offset) ->
    add_offset(Datetime, {0,0,0}, Offset).
add_offset(Datetime, FromOffset, ToOffset) ->
    FromSec= calendar:time_to_seconds(FromOffset),
    ToSec= calendar:time_to_seconds(ToOffset),
    add_seconds(Datetime, ToSec -FromSec).



% returns {WallTime, StdTime, UtcTime}
% where each is a datetime tuple: {{Y,M,D}{HH,MM,SS}}


% @todo ensure X is in list: max, maximum, min, minimum, current, ...?
all_times(X,_,_) when is_atom(X) ->
    {X,X,X};

% universal time given
all_times({Date, #tztime{time=UTCTime, flag=Flag}}, Offset, DSTOffset)
  when Flag=:=u; Flag=:=g; Flag=:=z ->
    UTCDatetime= {Date, UTCTime},

    OSec= calendar:time_to_seconds(Offset),
    DSTSec= calendar:time_to_seconds(DSTOffset),

    STDTime= add_seconds(UTCDatetime, OSec),
    WallTime= add_seconds(STDTime, DSTSec),

    {WallTime, STDTime, UTCDatetime};


% standard time given
all_times({Date, #tztime{time=STDTime, flag=s}}, Offset, DSTOffset) ->
    STDDatetime= {Date, STDTime},

    OSec= calendar:time_to_seconds(Offset),
    DSTSec= calendar:time_to_seconds(DSTOffset),

    UTCTime= add_seconds(STDDatetime, -1*OSec),
    WallTime= add_seconds(STDDatetime, DSTSec),

    {WallTime, STDDatetime, UTCTime};


% wall time given
all_times({Date, #tztime{time=WallTime, flag=Flag}}, Offset, DSTOffset)
  when Flag=:=w; Flag=:=undefined ->
    WallDatetime= {Date, WallTime},

    OSec= calendar:time_to_seconds(Offset),
    DSTSec= calendar:time_to_seconds(DSTOffset),

    STDTime= add_seconds(WallDatetime, -1*DSTSec),
    UTCTime= add_seconds(STDTime, -1*OSec),

    {WallDatetime, STDTime, UTCTime}.




compare(_, current) ->
    true;
compare(current, _) ->
    false;


compare(_, X) when X=:=max; X=:=maximum ->
    true;
compare(X, _) when X=:=max; X=:=maximum ->
    false;

compare(X, _) when X=:=min; X=:=minimum ->
    true;
compare(_, X) when X=:=min; X=:=minimum ->
    false;


% returns true if DT1 =< DT2. False otherwise. can be used with lists:sort/2
% both times are assumed to be in the same zone/DST context
compare(DT1={{Y1,M1,D1},{HH1,MM1,SS1}}, DT2={{Y2,M2,D2},{HH2,MM2,SS2}})
when is_integer(Y1), is_integer(Y2)
     , is_integer(M1), is_integer(M2)
     , is_integer(D1), is_integer(D2)
     , is_integer(HH1), is_integer(HH2)
     , is_integer(MM1), is_integer(MM2)
     , is_integer(SS1), is_integer(SS2)
     ->

    DT1 =< DT2;


compare({Date1, #tztime{time=Time1, flag=F}}
	, {Date2, #tztime{time=Time2, flag=F}}) ->

    Date1 =< Date2 orelse Time1 =< Time2;

compare(X,Y) ->
    erlang:error(bad_dates, [X,Y]).


equal(X,Y) when X=:=Y ->
    true;
equal(_,_) ->
    false.



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



%% subtracts 1 second from a single datetime
% @todo type checking
m1s(Date= {{Y,M,D},{HH,MM,SS}})
  when is_integer(Y), is_integer(M), is_integer(D)
     , is_integer(HH), is_integer(MM), is_integer(SS) ->

    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Date) - 1);

%% subtracts 1 second from all datetimes
% @todo type checking
m1s({WD, SD, UD}) ->
    {m1s(WD), m1s(SD), m1s(UD)}.

%% subtracts 1 second from all datetimes
% @todo type checking
m1s(W,S,U) when is_tuple(W), is_tuple(S), is_tuple(U) ->
    {m1s(W), m1s(S), m1s(U)}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


