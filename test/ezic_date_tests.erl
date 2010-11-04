-module(ezic_date_tests).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

prevnext_day_test_() ->
    [
     ?_assertEqual({2010,11,3}, ezic_date:previous_day("Wed", {2010, 11, 5}))
     , ?_assertEqual({2010,11,10}, ezic_date:next_day("Wed", {2010, 11, 5}))

     , ?_assertEqual({2010,11,5}, ezic_date:previous_day("Fri", {2010, 11, 5}))
     , ?_assertEqual({2010,11,5}, ezic_date:next_day("Fri", {2010, 11, 5}))

     , ?_assertEqual({2010,11,5}, ezic_date:previous_day("Fri", {2010, 11, 10}))
     , ?_assertEqual({2010,11,12}, ezic_date:next_day("Fri", {2010, 11, 10}))

     , ?_assertError(no_previous_day, ezic_date:previous_day("Wed", {2010, 11, 2}))
     , ?_assertError(no_next_day, ezic_date:next_day("Wed", {2010, 11, 25}))
    ].

firstlast_day_of_test_() ->
    [
     ?_assertEqual({2010,11,24}, ezic_date:last_day_of("Wed", 2010, 11))
     , ?_assertEqual({2010,11,30}, ezic_date:last_day_of("Tue", 2010, 11))

     , ?_assertEqual({2010,11,14}, ezic_date:first_day_limited("Sun", {geq, 8}, 2010, 11))
     , ?_assertEqual({2010,11,21}, ezic_date:first_day_limited("Sun", {leq, 25}, 2010, 11))
    ].


compare_datetimes_test_() ->
    [
     ?_assertEqual(false, ezic_date:compare_datetimes(
		       {{1997,3,lastSun},{tztime,{1,0,0},u}}
		       , {1997,1,1} ))
    ].
