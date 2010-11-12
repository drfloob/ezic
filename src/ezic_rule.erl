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



% returns the projected date of the next "rule event" in UTC time,
%  after the given UTCDatetime, with the given the zone Offset and current DST offset.
project_next(Rule=#rule{from=RFrom}, Offset, DSTOffset, minimum) ->
    ezic_date:for_rule_utc(Rule, Offset, DSTOffset, RFrom);
project_next(Rule=#rule{}, Offset, DSTOff, UTCAfter={{AY,_,_},_}) ->
    AllYears= years(Rule),
    GoodYears= years_after(AY, AllYears),
    project_next2(Rule, Offset, UTCAfter, DSTOff, GoodYears).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% returns the sorted list of years a rule existed for.
years(#rule{from=From, to=only}) ->
    [From];
years(#rule{from=From, to=To}) ->
    try lists:seq(From, To)
    catch error:function_clause ->
	    erlang:error(bad_year_range, [From, To])
    end.

% returns the years from the list that are greater than OR equal to Y
years_after(_, []) ->
    [];
years_after(Y, Years) ->
    {GOOD, _}= lists:partition(
      fun(X) when Y =< X -> true; 
	 (_) -> false 
      end, Years),
    GOOD.
			    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



project_next2(_,_,_,_,[]) ->
    none;
project_next2(Rule, Offset, UTCAfter, DSTOffset, [Year|RestYears]) ->
    RuleDate= ezic_date:for_rule_utc(Rule, Offset, DSTOffset, Year),
    case ezic_date:compare(UTCAfter, RuleDate) andalso (not ezic_date:equal(UTCAfter, RuleDate)) of
	true -> RuleDate;
	_ -> project_next2(Rule, Offset, UTCAfter, DSTOffset, RestYears)
    end.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests of Private Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

years_test_() ->
    [
     ?_assertEqual([1952], years(#rule{from=1952,to=only}))
     , ?_assertEqual([1952,1953], years(#rule{from=1952,to=1953}))
     , ?_assertEqual([1952,1953,1954,1955], years(#rule{from=1952,to=1955}))
     , ?_assertError(bad_year_range, years(#rule{from=1955,to=1952}))
    ].


% @todo type checking. lack of types worries me sometimes :(
years_after_test_() ->
    [
     ?_assertEqual([2010], years_after(2010, [2010]))
     , ?_assertEqual([2010,2011], years_after(2010, [2010,2011]))
     , ?_assertEqual([2011], years_after(2011, [2010,2011]))
     , ?_assertEqual([], years_after(2012, [2010,2011]))
     , ?_assertEqual([], years_after(2012, []))
    ].

-endif.
