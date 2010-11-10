-module(ezic_date_tests).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").


for_rule_test_() ->
    BR= #rule{at=#tztime{}},
    [
     % last days of months
     ?_assertEqual({{2010, 03, 28}, #tztime{}}, ezic_date:for_rule(BR#rule{in=3, on={last, "Sun"}}, 2010))
     , ?_assertEqual({{2010, 11, 30}, #tztime{}}, ezic_date:for_rule(BR#rule{in=11, on={last, "Tue"}}, 2010))
     , ?_assertEqual({{2010, 11, 24}, #tztime{}}, ezic_date:for_rule(BR#rule{in=11, on={last, "Wed"}}, 2010))

     % days =< or >= absolute dates in given month
     , ?_assertEqual({{2010, 11, 14}, #tztime{}}, ezic_date:for_rule(BR#rule{in=11, on=#tzon{day="Sun", filter={geq, 9}}}, 2010))
     , ?_assertEqual({{2010, 11, 7}, #tztime{}}, ezic_date:for_rule(BR#rule{in=11, on=#tzon{day="Sun", filter={leq, 9}}}, 2010))

     % these should fail
     , ?_assertError(no_previous_day, ezic_date:for_rule(BR#rule{in=11, on=#tzon{day="Sun", filter={leq, 1}}}, 2010))
     , ?_assertError(no_next_day, ezic_date:for_rule(BR#rule{in=11, on=#tzon{day="Sun", filter={geq, 29}}}, 2010))
    ].


for_rule_utc_dst_test_() ->
    WRule= #rule{in=11, on=11, at=#tztime{time={0,0,0}}, save={0,0,0}},
    SRule= #rule{in=11, on=11, at=#tztime{time={0,0,0}, flag=s}, save={0,0,0}},
    URule= #rule{in=11, on=11, at=#tztime{time={0,0,0}, flag=u}, save={0,0,0}},
    DST= {1,0,0},

%%% given a DST savings of 1 in a -7 zone, and given rules in each of the relative timezones:
%%%   - wall time should respect the given DST and offset
%%%   - std time should respect the offset, but not DST
%%%   - utc should respect neither
    [
     %% same "time" with different relative contexts should return different UTC results
     ?_assertEqual({{2010,11,11}, {6,0,0}}, ezic_date:for_rule_utc(WRule, {-7,0,0}, DST, 2010))
     , ?_assertEqual({{2010,11,11}, {7,0,0}}, ezic_date:for_rule_utc(SRule, {-7,0,0}, DST, 2010))
     , ?_assertEqual({{2010,11,11}, {0,0,0}}, ezic_date:for_rule_utc(URule, {-7,0,0}, DST, 2010))

     %% some absolute time in all contexts should return the same UTC results
     , ?_assertEqual({{2010,11,11}, {0,0,0}}, 
		     ezic_date:for_rule_utc(WRule#rule{on=10, at=#tztime{flag=w, time={18,0,0}}}, {-7,0,0}, DST, 2010))
     , ?_assertEqual({{2010,11,11}, {0,0,0}}, 
		     ezic_date:for_rule_utc(SRule#rule{on=10, at=#tztime{flag=s, time={17,0,0}}}, {-7,0,0}, DST, 2010))
     , ?_assertEqual({{2010,11,11}, {0,0,0}}, 
		     ezic_date:for_rule_utc(URule, {-7,0,0}, DST, 2010))
    ].


add_seconds_test_() ->
    [
     ?_assertEqual({{2010,11,7},{9,38,01}}, ezic_date:add_seconds({{2010,11,7},{9,38,00}}, 1))
     , ?_assertEqual({{2010,11,7},{9,38,00}}, ezic_date:add_seconds({{2010,11,7},{9,37,59}}, 1))

     , ?_assertEqual({{2010,11,7},{9,37,59}}, ezic_date:add_seconds({{2010,11,7},{9,38,00}}, -1))
     , ?_assertError(baddate, ezic_date:add_seconds({{2010,11,nope},{9,38,00}}, -1))
     , ?_assertError(baddate, ezic_date:add_seconds({2010,11,nope}, -1))
    ].


