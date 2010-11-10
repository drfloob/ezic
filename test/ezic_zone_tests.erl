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



project_end_test_() ->
    [
     ?_assertEqual({{2010,11,10},{16,20,0}}, 
		   ezic_zone:project_end(
		     #zone{until={{2010,11,10},#tztime{time={10,20,0}, flag=w}}, gmtoff={-7,0,0}}
		    , {1,0,0}))

     , ?_assertEqual({{2010,11,10},{16,20,0}}, 
		   ezic_zone:project_end(
		     #zone{until={{2010,11,10},#tztime{time={9,20,0}, flag=s}}, gmtoff={-7,0,0}}
		    , {1,0,0}))

     , ?_assertEqual({{2010,11,10},{16,20,0}}, 
		   ezic_zone:project_end(
		     #zone{until={{2010,11,10},#tztime{time={16,20,0}, flag=u}}, gmtoff={-7,0,0}}
		    , {1,0,0}))
    ].
