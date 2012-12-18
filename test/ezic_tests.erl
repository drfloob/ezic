-module(ezic_tests).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

local_to_utc_errors_test_() ->
    [
     ?_assertMatch({error, {ambiguous_zone, _}}, ezic:local_to_utc({{2010,11,7},{1,0,0}}, "America/Los_Angeles"))
     , ?_assertEqual({error, no_zone}, ezic:local_to_utc({{2010,3,14},{2,30,0}}, "America/Los_Angeles"))
    ].

utc_to_local_smoke_test_() ->
    [?_assertEqual({{2012,12,17},{13,20,0}}, ezic:utc_to_local({{2012,12,17},{4,20,0}}, "Asia/Tokyo"))
    ].
