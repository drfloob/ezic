-module(ezic_date_tests).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").


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



for_rule_test_() ->
    IrkutskRule1= #rule{from=1981, to=1984, in=4, on=1, at=#tztime{}, save={1,0,0}},
    IrkutskRule2= #rule{from=1981, to=1984, in=10, on=1, at=#tztime{}, save={0,0,0}},
    
    [
     %% Asia/Irkutsk
     ?_assertEqual({
		    { {{1981,4,1},{0,0,0}}, {{1981,4,1},{1,0,0}} },   % {oldDst, newDst} 
		    {{1981,4,1},{0,0,0}}, 
		    {{1981,3,31},{16,0,0}}
		   },
		  ezic_date:for_rule(IrkutskRule1, {8,0,0}, {0,0,0}, {1,0,0}, 1981))

     %% Another Asia/Irkutsk. ezic_date:add_offset repaired.
     , ?_assertEqual({
		    { {{1981,10,1},{0,0,0}}, {{1981,9,30},{23,0,0}} },   % {oldDst, newDst} 
		    {{1981,9,30},{23,0,0}}, 
		    {{1981,9,30},{15,0,0}}
		   },
		  ezic_date:for_rule(IrkutskRule2, {8,0,0}, {1,0,0}, {0,0,0}, 1981))

     %% Africa/Tripoli recursion bug
     , ?_assertEqual({
		    { {{1952,1,1},{0,0,0}}, {{1951,12,31},{23,0,0}} },   % {oldDst, newDst} 
                    {{1951,12,31},{23,0,0}},
                    {{1951,12,31},{22,0,0}} },
		  ezic_date:for_rule(
		    #rule{from=1952, to=only, in=1, on=1, at=#tztime{}, save={0,0,0}}, 
		    {1,0,0}, {1,0,0}, {0,0,0}, 1952))

    ].



add_seconds_test_() ->
    [
     ?_assertEqual({{2010,11,7},{9,38,01}}, ezic_date:add_seconds({{2010,11,7},{9,38,00}}, 1))
     , ?_assertEqual({{2010,11,7},{9,38,00}}, ezic_date:add_seconds({{2010,11,7},{9,37,59}}, 1))

     , ?_assertEqual({{2010,11,7},{9,37,59}}, ezic_date:add_seconds({{2010,11,7},{9,38,00}}, -1))
     , ?_assertError(baddate, ezic_date:add_seconds({{2010,11,nope},{9,38,00}}, -1))
     , ?_assertError(baddate, ezic_date:add_seconds({2010,11,nope}, -1))
    ].




compare_test_() ->
    [
     ?_assert(ezic_date:compare({2011,12,12}, current))
     , ?_assertNot(ezic_date:compare(current, {2099,12,12})) % is this the right behavior?
    ].




all_times_test_() ->
    
    Date={2010,11,12},
    UTC=#tztime{time={16,2,0}, flag=u},
    
    [
     ?_assertEqual(
	{{Date,{9,2,0}}, {Date,{8,2,0}}, {Date,{16,2,0}}}
	, ezic_date:all_times({Date,UTC}, {-8,0,0}, {1,0,0}))
    ].



normalize_test_() ->
    TZT= #tztime{time={1,2,3}, flag=s},
    [
     ?_assertEqual({{2010,1,1}, #tztime{}}, ezic_date:normalize(2010))
     , ?_assertEqual({{2010,2,3}, #tztime{}}, ezic_date:normalize({2010,2,3}))
     , ?_assertEqual({{2010,11,28}, #tztime{}}, ezic_date:normalize({2010,11,{last, "Sun"}}))
     , ?_assertEqual({{2010,11,16}, #tztime{}}, ezic_date:normalize({2010,11,#tzon{day="Tue", filter={geq, 10}}}))

     , ?_assertError(function_clause, ezic_date:normalize({2010, #tztime{}}))

     , ?_assertEqual({{2010,2,3}, TZT}, ezic_date:normalize({{2010,2,3}, TZT}))
     , ?_assertEqual({{2010,11,28}, TZT}, ezic_date:normalize({{2010,11,{last, "Sun"}}, TZT}))
     , ?_assertEqual({{2010,11,16}, TZT}, ezic_date:normalize({{2010,11,#tzon{day="Tue", filter={geq, 10}}}, TZT}))

    ].



assert_valid_test_() ->
    [
     ?_assertEqual(ok, ezic_date:assert_valid({{2013, 02, 28}, {0,0,0}}))
     , ?_assertMatch({error, {baddate, _}}, ezic_date:assert_valid({{2013, 02, 29}, {0,0,0}}))

     , ?_assertEqual(ok, ezic_date:assert_valid({{2013, 02, 28}, {0,0,0}}))
     , ?_assertMatch({error, {baddate, _}}, ezic_date:assert_valid({{2013, 02, 28}, {a,0,0}}))

     %% an oddity in date validity: negative time is OK ...
     , ?_assertMatch(ok, ezic_date:assert_valid({{2013, 02, 28}, {-1,0,0}}))
     %% ... but negative dates are not
     , ?_assertMatch({error, {baddate, _}}, ezic_date:assert_valid({{2013, 02, -1}, {0,0,0}}))
    ].
