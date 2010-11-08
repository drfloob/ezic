-module(ezic_date_tests).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").


for_rule_test_() ->
    [
     % last days of months
     ?_assertEqual({2010, 03, 28}, ezic_date:for_rule(#rule{in=3, on={last, "Sun"}}, 2010))
     , ?_assertEqual({2010, 11, 30}, ezic_date:for_rule(#rule{in=11, on={last, "Tue"}}, 2010))
     , ?_assertEqual({2010, 11, 24}, ezic_date:for_rule(#rule{in=11, on={last, "Wed"}}, 2010))

     % days =< or >= absolute dates in given month
     , ?_assertEqual({2010, 11, 14}, ezic_date:for_rule(#rule{in=11, on=#tzon{day="Sun", filter={geq, 9}}}, 2010))
     , ?_assertEqual({2010, 11, 7}, ezic_date:for_rule(#rule{in=11, on=#tzon{day="Sun", filter={leq, 9}}}, 2010))

     % these should fail
     , ?_assertError(no_previous_day, ezic_date:for_rule(#rule{in=11, on=#tzon{day="Sun", filter={leq, 1}}}, 2010))
     , ?_assertError(no_next_day, ezic_date:for_rule(#rule{in=11, on=#tzon{day="Sun", filter={geq, 29}}}, 2010))
    ].


add_seconds_test_() ->
    [
     ?_assertEqual({{2010,11,7},{9,38,01}}, ezic_date:add_seconds({{2010,11,7},{9,38,00}}, 1))
     , ?_assertEqual({{2010,11,7},{9,38,00}}, ezic_date:add_seconds({{2010,11,7},{9,37,59}}, 1))

     , ?_assertEqual({{2010,11,7},{9,37,59}}, ezic_date:add_seconds({{2010,11,7},{9,38,00}}, -1))
     , ?_assertError(baddate, ezic_date:add_seconds({{2010,11,nope},{9,38,00}}, -1))
     , ?_assertError(baddate, ezic_date:add_seconds({2010,11,nope}, -1))
    ].


