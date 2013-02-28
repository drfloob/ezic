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



project_end_utc_test_() ->
    [
     ?_assertEqual({{2010,11,10},{16,20,0}},
		   ezic_zone:project_end_utc(
		     #zone{until={{2010,11,10},#tztime{time={10,20,0}, flag=w}}, gmtoff={-7,0,0}}
		    , {1,0,0}))

     , ?_assertEqual({{2010,11,10},{16,20,0}},
		   ezic_zone:project_end_utc(
		     #zone{until={{2010,11,10},#tztime{time={9,20,0}, flag=s}}, gmtoff={-7,0,0}}
		    , {1,0,0}))

     , ?_assertEqual({{2010,11,10},{16,20,0}},
		   ezic_zone:project_end_utc(
		     #zone{until={{2010,11,10},#tztime{time={16,20,0}, flag=u}}, gmtoff={-7,0,0}}
		    , {1,0,0}))


     %% previously failing for "Asia/Irkutsk"
     , ?_assertEqual({{1879,12,31},{17,2,40}}, ezic_zone:project_end_utc(
					#zone{until={1880,1,1}, gmtoff={6,57,20}}, {0,0,0}))
    ].



next_datetime_test_() ->
    BZ=#zone{gmtoff={0,0,0}},
    AllZones= [
	       Z1=BZ#zone{until={{2010,11,15},#tztime{}}}
	       , Z2=BZ#zone{until={{2010,11,5},#tztime{}}}
	       , Z3=BZ#zone{until={{2010,11,25},#tztime{}}}

	       , Zt1=BZ#zone{until={{2010,11,5},#tztime{time={5,0,0}, flag=u}}}
	       , Zt2=BZ#zone{until={{2010,11,5},#tztime{time={12,0,0}, flag=u}}}
	       , Zt3=BZ#zone{until={{2010,11,5},#tztime{time={17,0,0}, flag=u}}}

	       , Zy1=BZ#zone{until={{1978,11,5},#tztime{}}}
	       , Zy2=BZ#zone{until={{1979,11,5},#tztime{}}}
	       , Zy3=BZ#zone{until={{1980,11,5},#tztime{}}}
	    ],
    [
     % day limits
     ?_assertMatch([Z2|_], ezic_zone:next(AllZones, {{2010,11,1},{0,0,0}}, {0,0,0}))
     , ?_assertMatch([Z1|_], ezic_zone:next(AllZones, {{2010,11,6},{0,0,0}}, {0,0,0}))
     , ?_assertMatch([Z3|_], ezic_zone:next(AllZones, {{2010,11,16},{0,0,0}}, {0,0,0}))

     % time limits
     , ?_assertMatch([Zt1|_], ezic_zone:next(AllZones, {{2010,11,5},{0,0,1}}, {0,0,0}))
     , ?_assertMatch([Zt2|_], ezic_zone:next(AllZones, {{2010,11,5},{5,0,1}}, {0,0,0}))
     , ?_assertMatch([Zt3|_], ezic_zone:next(AllZones, {{2010,11,5},{12,0,1}}, {0,0,0}))

     % year tests
     , ?_assertMatch([Zy1|_], ezic_zone:next(AllZones, {{1978,11,5},{0,0,0}}, {0,0,0}))
     , ?_assertMatch([Zy2|_], ezic_zone:next(AllZones, {{1978,11,7},{0,0,0}}, {0,0,0}))
     , ?_assertMatch([Zy3|_], ezic_zone:next(AllZones, {{1979,11,8},{0,0,0}}, {0,0,0}))

    ].

%% Take 3 zones identical aside from their "time-relativity", each
%% ending in standard, wall, and universal time, the following tests
%% show the choice of "next soonest" as deterministic.
next_relative_time_test_() ->
    Zones= [
	    ZW=#zone{until={{2010,11,7},#tztime{time={2,0,0}, flag=w}}, gmtoff={-7,0,0}}
	    , ZS=#zone{until={{2010,11,7},#tztime{time={2,0,0}, flag=s}}, gmtoff={-7,0,0}}
	    , ZU=#zone{until={{2010,11,7},#tztime{time={2,0,0}, flag=u}}, gmtoff={-7,0,0}}
	   ],
    [
     %% same dst, different from
     ?_assertMatch([ZW|_], ezic_zone:next(Zones,{{2010,11,7},{6,0,0}},{2,0,0}))
     , ?_assertMatch([ZS|_], ezic_zone:next(Zones,{{2010,11,7},{7,0,1}},{2,0,0}))
     , ?_assertMatch([ZU|_], ezic_zone:next(Zones,{{2010,11,7},{1,0,1}},{2,0,0}))

     %% same from, different dst
     , ?_assertMatch([ZW|_], ezic_zone:next(Zones,{{2010,11,7},{8,50,0}},{0,9,0})) %1:59wt, 1:50st
     , ?_assertMatch([ZS|_], ezic_zone:next(Zones,{{2010,11,7},{8,50,0}},{0,-1,0})) %1:49wt, 1:50st
    ].
