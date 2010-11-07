-module(ezic_rule).
-include("include/ezic.hrl").


-export([current/2, current_set/2, sort/2]).
-export([from_time/1, dst_sec/1]).



current(_, []) ->
    none;
current(Now, Rules) ->
    Current_SetRules= lists:filter(fun(R)-> current_set(Now, R) end, Rules),
%    ?debug("Current_SetRules: ~p", [Current_SetRules]),
    SRules= lists:sort(fun sort/2, Current_SetRules),
    CRule= choose_rule(SRules),
    CRule.




% newest first
sort(R1, R2) ->
    T1= from_time(R1),
    T2= from_time(R2),
    ezic_date:compare_datetimes(T2, T1).



current_set(Now, Rule=#rule{from=F, to=T}) ->
    {{Y,_,_},_} = Now,
    case ezic_date:date_between(Y, {F,T}) of
	false -> false;
	true -> current_set2(Now, Rule)
    end.


current_set2(Now, #rule{in=Month, on=Day, at=Time}) ->
    {{Y,_,_},_} = Now,
    RTime= {{Y, Month, Day}, Time},
    ezic_date:compare_datetimes(RTime, Now).



from_time(#rule{from=F, in=M, on=O, at=A}) ->
    {{F,M,O},A}.





choose_rule([]) ->
    none;
choose_rule([H|_]) ->
    H.


dst_sec(none) ->
    0;
dst_sec(Rule) ->
    calendar:time_to_seconds((Rule#rule.save)#tztime.time).


