-module(ezic_rule_tests).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").


project_next_test_() ->
    [
     
     % simple UTC example
     ?_assertEqual({{2010,11,10},{16,20,0}}, 
		   ezic_rule:project_next(
		     #rule{from=1984, to=2032, in=11, on=10, at=#tztime{time={16,20,0}, flag=u}}
		     , {-5,0,0}, {-1,0,0}, {{2010,11,10},{0,0,0}}
		    ))

     % skip a year UTC example
     , ?_assertEqual({{2011,11,10},{16,20,0}}, 
		   ezic_rule:project_next(
		     #rule{from=1984, to=2032, in=11, on=10, at=#tztime{time={16,20,0}, flag=u}}
		     , {-5,0,0}, {-1,0,0}, {{2010,11,10},{16,21,0}}
		    ))

     % skip 3 years UTC example
     , ?_assertEqual({{2013,11,10},{16,20,0}}, 
		   ezic_rule:project_next(
		     #rule{from=1984, to=2032, in=11, on=10, at=#tztime{time={16,20,0}, flag=u}}
		     , {-5,0,0}, {-1,0,0}, {{2012,11,11},{16,20,0}}
		    ))

     % no solution
     , ?_assertEqual(none, 
		   ezic_rule:project_next(
		     #rule{from=1984, to=2010, in=11, on=10, at=#tztime{time={16,20,0}, flag=u}}
		     , {-5,0,0}, {-1,0,0}, {{2012,11,11},{16,20,0}}
		    ))

    ].
