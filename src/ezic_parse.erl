-module(ezic_parse).
-include("include/ezic.hrl").


-export([
	 year/1
	 , day_pattern/1
	 , time/1
	 , time_val/1
	 , until/1
	 , tz_abbr/1
	]).



% year(string) to year(int|atom)
year("minimum") -> minimum;
year("min") -> minimum;
year("maximum") -> maximum;
year("max") -> maximum;
year("only") -> only;
year(X) ->
    list_to_integer(X).



% X=last(Mon,Tue,Wed,...,Sun) -> {last, (Mon,Tue,Web,...,Sun)}
day_pattern(X=[$l,$a,$s,$t|Day]) ->
    case ezic_date:day_to_num(Day) > 0 of
	true -> {last, Day};
	false -> erlang:error(badday,X)
    end;
% X=(Mon,Tue,Wed,...,Sun)[<>]=int().
day_pattern(X=[D,A,Y, Sign,$= | IntS]) ->
    Int=list_to_integer(IntS),
    Day=[D,A,Y],
    {ok, FSign}= sign(Sign),

    case ezic_date:day_to_num(Day) > 0 of
	true -> #tzon{day=Day, filter={FSign, Int}};
	false -> erlang:error(badday, X)
    end;
% X=int()
day_pattern(X) ->
    list_to_integer(X).




time_val(Str) ->
    #tztime{time=Time}= time(Str),
    Time.

time("-") ->
    #tztime{time={0,0,0}};
time(TS) ->
    {Flag, RTime} = time_flag(TS),
    Time= time_abs(RTime),
    #tztime{time=Time, flag=Flag}.



until([]) ->
    current;
until([YS]) ->
    Y= list_to_integer(YS),
    {Y,1,1};
until([YS,MS]) ->
    Y= list_to_integer(YS),
    M= ezic_date:month_to_num(MS),
    {Y,M,1};
until([YS,MS,DS]) ->
    Y= list_to_integer(YS),
    M= ezic_date:month_to_num(MS),
    D= day_pattern(DS),
    {Y,M,D};
until([YS,MS,DS,TS]) ->
    Y= list_to_integer(YS),
    M= ezic_date:month_to_num(MS),
    D= day_pattern(DS),
    {{Y,M,D},ezic_parse:time(TS)}.




tz_abbr(Format) ->
    tz_abbr(Format, string:str(Format, "%s")).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



time_flag(TS) ->
    {F, RTime}= time_flag_rev(lists:reverse(TS)),
    {F, lists:reverse(RTime)}.


% absolute time string (no relativity modifiers)
% e.g. 08:01:03
time_abs(Time) ->
    TTok= string:tokens(Time, ":"),
    time_tokens(TTok).


time_tokens([HS]) ->
    H= list_to_integer(HS),
    {H,0,0};
time_tokens([HS,MS]) ->
    H= list_to_integer(HS),
    M= list_to_integer(MS),
    {H,M,0};
time_tokens([HS,MS,SS]) ->
    H= list_to_integer(HS),
    M= list_to_integer(MS),
    S= list_to_integer(SS),
    {H,M,S};
time_tokens(X) ->
    erlang:error(badtime, X).




sign($>) -> {ok, geq};
sign($<) -> {ok, leq}.







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


time_flag_rev([F|RTime]) when F =:= $s; F =:= $w; F =:= $u ->
    {list_to_atom([F]), RTime};
time_flag_rev([F|RTime]) when F =:= $g; F =:= $z ->
    {u, RTime};
time_flag_rev(X=[F|_]) when F-48 < 0; F-48 > 9 ->
    erlang:error(badtime, lists:reverse(X));
time_flag_rev(RTime) ->
    {w, RTime}.



tz_abbr(Format, 0) ->
    Format;
tz_abbr(Format, Pos) ->
    First=string:substr(Format, 1, Pos-1),
    Last=string:substr(Format, Pos+2),
    string:concat(First, [$~, $s|Last]).





