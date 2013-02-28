-module(ezic_rule).
-include("include/ezic.hrl").


-export([parse/1]).
-export([project_next/4]).



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



% returns {Year, UTCDate} where UTCDate is the projected date of the
% next "rule event" in UTC time, after the given UTCDatetime, with the
% given the zone Offset and current DST offset, and Year is the rule
% year (which may differ from the UTC year).
project_next(Rule=#rule{from=RFrom}, Offset, DSTOffset, minimum) ->
    RetDate={{RetYear,_,_},_}= ezic_date:for_rule_utc(Rule, Offset, DSTOffset, RFrom),
    {RetYear, RetDate};
project_next(Rule=#rule{}, Offset, DSTOff, UTCAfter={{AY,_,_},_}) ->
    YearRange= years(Rule),
    GoodYears= years_after(AY, YearRange),
    project_next2(Rule, Offset, UTCAfter, DSTOff, GoodYears).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% returns the range of years a rule existed for, normalized for calculations.
years(#rule{from=From, to=only}) ->
    {From,From};
years(#rule{from=From, to=X}) when X=:=max; X=:=maximum->
    {From,X};
years(#rule{from=From, to=To}) when From =< To, is_integer(From), is_integer(To) ->
    {From, To};
years(#rule{from=From, to=To}) ->
    erlang:error(bad_year_range, [From, To]).

%% returns the range of years that are greater than OR equal to Y.
%% calculation stops after a fixed year, configurable above.
years_after(Y, {F,M}) when M=:=max; M=:=maximum ->
    {erlang:max(Y,F), M};
years_after(Y, {_From, To}) when Y > To ->
    none;
years_after(Y, {From, To}) ->
    {erlang:max(Y,From), To}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


project_next2(_,_,_,_,none) ->
    none;
project_next2(Rule, Offset, UTCAfter, DSTOffset, Years={FromYear, _}) ->
    RuleDate= ezic_date:for_rule_utc(Rule, Offset, DSTOffset, FromYear),
    case ezic_date:compare(UTCAfter, RuleDate) andalso (not ezic_date:equal(UTCAfter, RuleDate)) of
	true -> {FromYear, RuleDate};
	false ->
	    RestYears= years_after(FromYear+1, Years),
	    project_next2(Rule, Offset, UTCAfter, DSTOffset, RestYears)
    end.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests of Private Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

years_test_() ->
    [
     ?_assertEqual({1952,1952}, years(#rule{from=1952,to=only}))
     , ?_assertEqual({1952,1953}, years(#rule{from=1952,to=1953}))
     , ?_assertEqual({1952,1955}, years(#rule{from=1952,to=1955}))

     , ?_assertEqual({1952,max}, years(#rule{from=1952,to=max}))
     , ?_assertEqual({1952,maximum}, years(#rule{from=1952,to=maximum}))

     , ?_assertError(bad_year_range, years(#rule{from=1955,to=1952}))
    ].


% @todo type checking. lack of types worries me sometimes :(
years_after_test_() ->
    [
     ?_assertEqual({2010,2010}, years_after(2010, {2010,2010}))
     , ?_assertEqual({2010,2011}, years_after(2010, {2010,2011}))
     , ?_assertEqual({2011,2011}, years_after(2011, {2010,2011}))

     , ?_assertEqual({2011,max}, years_after(2011, {2010,max}))
     , ?_assertEqual({2010,max}, years_after(2009, {2010,max}))

     , ?_assertEqual(none, years_after(2012, {2010,2011}))
    ].

-endif.
