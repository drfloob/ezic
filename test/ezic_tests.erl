-module(ezic_tests).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

smoke_test_() ->
    [
     %% utc_to_local - normal tests
     ?_assertEqual({{2012,12,17},{13,20,0}}, ezic:utc_to_local({{2012,12,17},{4,20,0}}, "Asia/Tokyo"))
     , ?_assertEqual({{2012,12,16},{23,20,0}}, ezic:utc_to_local({{2012,12,17},{4,20,0}}, "America/Jamaica"))
     , ?_assertEqual({{2012,12,16},{20,20,0}}, ezic:utc_to_local({{2012,12,17},{4,20,0}}, "America/Los_Angeles"))

     %% local_to_utc - normal tests
     , ?_assertEqual({{2012,12,17},{4,20,0}}, ezic:local_to_utc({{2012,12,17},{13,20,0}}, "Asia/Tokyo"))
     , ?_assertEqual({{2012,12,17},{4,20,0}}, ezic:local_to_utc({{2012,12,16},{23,20,0}}, "America/Jamaica"))
     , ?_assertEqual({{2012,12,17},{4,20,0}}, ezic:local_to_utc({{2012,12,16},{20,20,0}}, "America/Los_Angeles"))
    ].


local_to_utc_errors_test_() ->
    [
     ?_assertMatch({error, {ambiguous_zone, _}}, ezic:local_to_utc({{2010,11,7},{1,0,0}}, "America/Los_Angeles"))
     , ?_assertEqual({error, no_zone}, ezic:local_to_utc({{2010,3,14},{2,30,0}}, "America/Los_Angeles"))
    ].


%% Tests overlap logic around timezones that have odd DST rules
local_to_utc_funkyDST_test_() ->
    [
     %% gap
     ?_assertMatch({{1948,5,1},{16,0,0}}, ezic:local_to_utc({{1948,5,2},{1,00,00}}, "Asia/Tokyo"))
     , ?_assertMatch({error, no_zone}, ezic:local_to_utc({{1948,5,2},{2,00,00}}, "Asia/Tokyo"))
     , ?_assertMatch({{1948,5,1},{17,0,0}}, ezic:local_to_utc({{1948,5,2},{3,00,00}}, "Asia/Tokyo"))

     %% overlap
     , ?_assertMatch({{1948,9,10},{14,30,0}}, ezic:local_to_utc({{1948,9,11},{0,30,00}}, "Asia/Tokyo"))
     , ?_assertMatch({error, {ambiguous_zone, _}}, ezic:local_to_utc({{1948,9,11},{1,00,00}}, "Asia/Tokyo"))
     , ?_assertMatch({{1948,9,10},{17,0,0}}, ezic:local_to_utc({{1948,9,11},{2,00,00}}, "Asia/Tokyo"))

     %% DST end
     , ?_assertMatch({error, no_zone}, ezic:local_to_utc({{1951,5,6},{2,0,0}}, "Asia/Tokyo")) %% final DST gap
     , ?_assertMatch({{1951,5,5},{17,0,0}}, ezic:local_to_utc({{1951,5,6},{3,0,0}}, "Asia/Tokyo")) %% DST in effect for the last time
     , ?_assertMatch({error, {ambiguous_zone, _}}, ezic:local_to_utc({{1951,9,8},{1,0,0}}, "Asia/Tokyo")) %% DST overlap
     , ?_assertMatch({{1951,9,7},{17,0,0}}, ezic:local_to_utc({{1951,9,8},{2,0,0}}, "Asia/Tokyo")) %% DST off for the last time
     , ?_assertMatch({{1952,5,3},{17,0,0}}, ezic:local_to_utc({{1952,5,4},{2,0,0}}, "Asia/Tokyo")) %% ensure DST not in effect for 1952
    ].
