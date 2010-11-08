-module(ezic_parse_tests).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").


year_test_() ->
    [
     ?_assertEqual(minimum, ezic_parse:year("minimum"))
     , ?_assertEqual(maximum, ezic_parse:year("maximum"))
     , ?_assertEqual(only, ezic_parse:year("only"))
     , ?_assertEqual(1978, ezic_parse:year("1978"))
     , ?_assertError(badarg, ezic_parse:year("asdf"))
    ].



time_test_() ->
    [
     ?_assertEqual(#tztime{time={0,0,0}}, ezic_parse:time("-"))
     , ?_assertEqual(#tztime{time={2,0,0}}, ezic_parse:time("2:00"))
     , ?_assertEqual(#tztime{time={15,0,0}}, ezic_parse:time("15:00"))
     , ?_assertEqual(#tztime{time={1,28,14}}, ezic_parse:time("1:28:14"))
     , ?_assertEqual(#tztime{time={1,28,14}, flag=w}, ezic_parse:time("1:28:14w"))
     , ?_assertEqual(#tztime{time={1,28,14}, flag=s}, ezic_parse:time("1:28:14s"))
     , ?_assertEqual(#tztime{time={1,28,14}, flag=u}, ezic_parse:time("1:28:14u"))
     , ?_assertEqual(#tztime{time={1,28,14}, flag=u}, ezic_parse:time("1:28:14g"))
     , ?_assertEqual(#tztime{time={1,28,14}, flag=u}, ezic_parse:time("1:28:14z"))

     , ?_assertError(badtime, ezic_parse:time("1:28:14wtf?"))
     , ?_assertError(badtime, ezic_parse:time("1:28:14:00"))
    ].


time_val_test_() ->
    [
     ?_assertEqual({0,0,0}, ezic_parse:time_val("-"))
     , ?_assertEqual({2,0,0}, ezic_parse:time_val("2:00"))
     , ?_assertEqual({15,0,0}, ezic_parse:time_val("15:00"))
     , ?_assertEqual({1,28,14}, ezic_parse:time_val("1:28:14"))
     , ?_assertEqual({1,28,14}, ezic_parse:time_val("1:28:14w"))
     , ?_assertEqual({1,28,14}, ezic_parse:time_val("1:28:14s"))
     , ?_assertEqual({1,28,14}, ezic_parse:time_val("1:28:14u"))
     , ?_assertEqual({1,28,14}, ezic_parse:time_val("1:28:14g"))
     , ?_assertEqual({1,28,14}, ezic_parse:time_val("1:28:14z"))

     , ?_assertError(badtime, ezic_parse:time_val("1:28:14wtf?"))
     , ?_assertError(badtime, ezic_parse:time_val("1:28:14:00"))
    ].



day_pattern_test_() ->
    [
     ?_assertEqual(5, ezic_parse:day_pattern("5"))

     , ?_assertEqual({last, "Sun"}, ezic_parse:day_pattern("lastSun"))
     , ?_assertEqual({last, "Mon"}, ezic_parse:day_pattern("lastMon"))
     , ?_assertError(badday, ezic_parse:day_pattern("lastFoo"))

     , ?_assertEqual(#tzon{day="Sun", filter={geq, 8}}, ezic_parse:day_pattern("Sun>=8"))
     , ?_assertEqual(#tzon{day="Sun", filter={leq, 25}}, ezic_parse:day_pattern("Sun<=25"))
     , ?_assertError(badday, ezic_parse:day_pattern("Foo<=25"))
    ].




tz_abbr_test_() ->
    [
     ?_assertEqual("POS", ezic_parse:tz_abbr("POS"))
     , ?_assertEqual("P~sS", ezic_parse:tz_abbr("P%sS"))
     , ?_assertEqual("PS~s", ezic_parse:tz_abbr("PS%s"))
    ].



