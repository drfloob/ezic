-module(ezic_zone_tests).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

split_by_name_test_() ->
    AZones= [A=#zone{name=a}, #zone{name=a}, #zone{name=a}],
    BZones= [B=#zone{name=b}, #zone{name=b}, #zone{name=b}],
    AllZones= lists:merge(AZones, BZones),

    [
     ?_assertEqual({AZones, BZones}, ezic_zone:split_by_name(A, AllZones))
     , ?_assertEqual({BZones, AZones}, ezic_zone:split_by_name(B, AllZones))
     , ?_assertEqual({[], AllZones}, ezic_zone:split_by_name(#zone{name=c}, AllZones))
    ].
