-module(ezic_tests).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

local_to_utc_errors_test_() ->
    ezic_db:init(),
    [
     ?_assertError(ambiguous_zone, ezic:local_to_utc({{2010,11,7},{1,0,0}}, "America/Los_Angeles"))
     , ?_assertError(no_zone, ezic:local_to_utc({{2010,3,14},{2,30,0}}, "America/Los_Angeles"))
    ].
