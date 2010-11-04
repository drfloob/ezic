-module(ezic_record_tests).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").


parse_fromto_test_() ->
    [
     ?_assertEqual(minimum, ezic_record:parse_from("minimum"))
     , ?_assertEqual(minimum, ezic_record:parse_to("minimum"))

     , ?_assertEqual(maximum, ezic_record:parse_from("maximum"))
     , ?_assertEqual(maximum, ezic_record:parse_to("maximum"))

     , ?_assertError(badarg, ezic_record:parse_from("only"))
     , ?_assertEqual(only, ezic_record:parse_to("only"))

     , ?_assertEqual(1978, ezic_record:parse_from("1978"))
     , ?_assertEqual(1978, ezic_record:parse_to("1978"))

     , ?_assertError(badarg, ezic_record:parse_from("asdf"))
     , ?_assertError(badarg, ezic_record:parse_to("asdf"))
    ].


parse_time_test_() ->
    [
     ?_assertEqual(#tztime{time={0,0,0}}, ezic_record:parse_time("-"))
     , ?_assertEqual(#tztime{time={2,0,0}}, ezic_record:parse_time("2:00"))
     , ?_assertEqual(#tztime{time={15,0,0}}, ezic_record:parse_time("15:00"))
     , ?_assertEqual(#tztime{time={1,28,14}}, ezic_record:parse_time("1:28:14"))
     , ?_assertEqual(#tztime{time={1,28,14}, flag=u}, ezic_record:parse_time("1:28:14u"))
     , ?_assertEqual(#tztime{time={1,28,14}, flag=w}, ezic_record:parse_time("1:28:14w"))
     , ?_assertEqual(#tztime{time={1,28,14}, flag=s}, ezic_record:parse_time("1:28:14s"))

     , ?_assertError(badtime, ezic_record:parse_time("1:28:14wtf?"))
     , ?_assertError(badtime, ezic_record:parse_time("1:28:14:00"))
    ].


parse_at_test_() ->
    [
     ?_assertEqual(#tztime{time={0,0,0}}, ezic_record:parse_at("-"))
     , ?_assertEqual(#tztime{time={2,0,0}}, ezic_record:parse_at("2:00"))
     , ?_assertEqual(#tztime{time={15,0,0}}, ezic_record:parse_at("15:00"))
     , ?_assertEqual(#tztime{time={1,28,14}}, ezic_record:parse_at("1:28:14"))
     , ?_assertEqual(#tztime{time={1,28,14}, flag=u}, ezic_record:parse_at("1:28:14u"))
     , ?_assertEqual(#tztime{time={1,28,14}, flag=w}, ezic_record:parse_at("1:28:14w"))
     , ?_assertEqual(#tztime{time={1,28,14}, flag=s}, ezic_record:parse_at("1:28:14s"))
     , ?_assertError(badtime, ezic_record:parse_at("1:28:14wtf?"))
    ].


parse_save_test_() ->
    [
     ?_assertEqual(#tztime{time={0,0,0}}, ezic_record:parse_save("-"))
     , ?_assertEqual(#tztime{time={2,0,0}}, ezic_record:parse_save("2:00"))
     , ?_assertEqual(#tztime{time={15,0,0}}, ezic_record:parse_save("15:00"))
     , ?_assertEqual(#tztime{time={1,28,14}}, ezic_record:parse_save("1:28:14"))
     , ?_assertError(badsave, ezic_record:parse_save("1:28:14u"))
     , ?_assertError(badsave, ezic_record:parse_save("1:28:14w"))
     , ?_assertError(badsave, ezic_record:parse_save("1:28:14s"))
     , ?_assertError(badsave, ezic_record:parse_save("1:28:14wtf?"))
    ].





parse_on_test_() ->
    [
     ?_assertEqual(5, ezic_record:parse_on("5"))

     , ?_assertEqual({last, "Sun"}, ezic_record:parse_on("lastSun"))
     , ?_assertEqual({last, "Mon"}, ezic_record:parse_on("lastMon"))
     , ?_assertError(badday, ezic_record:parse_on("lastFoo"))

     , ?_assertEqual(#tzon{day="Sun", filter={geq, 8}}, ezic_record:parse_on("Sun>=8"))
     , ?_assertEqual(#tzon{day="Sun", filter={leq, 25}}, ezic_record:parse_on("Sun<=25"))
     , ?_assertError(badday, ezic_record:parse_on("Foo<=25"))
    ].


convert_format_test_() ->
    [
     ?_assertEqual("POS", ezic_record:convert_format("POS"))
     , ?_assertEqual("P~sS", ezic_record:convert_format("P%sS"))
    ].



