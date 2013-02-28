-module(ezic_flatten).
-include("include/ezic.hrl").


-define(FLAT(W,S,U), #flatzone{wall_from=W, std_from=S, utc_from=U}).
-define(ENDFLAT(F,W,S,U,D), F#flatzone{wall_to=W, std_to=S, utc_to=U, dstoffset=D}).
-define(MINFLAT, ?FLAT(minimum,minimum,minimum)).



%% @todo move MAXYEAR to config file
-define(MAXYEAR, 2500). % last year to process flatzones for.


-export([
	 flatten/2
	 , contains_date/2
	 , ms/2
	]).


flatten(Zones, AllRules) ->
    flatten_all_zones(Zones, AllRules).


contains_date(FlatZone, Date) ->
    NDate= ezic_date:normalize(Date),
    contains_date2(FlatZone, NDate).


%% create matchspec for the given date and name
%% date is expected to have a tztime with accurate flag
ms(Date, Name) ->

    M= #flatzone{tzname=Name
		 , wall_from='$1', wall_to='$2'
		 , std_from='$3', std_to='$4'
		 , utc_from='$5', utc_to='$6'
		 , _='_'},
    R= ['$_'],

    {D, #tztime{time=T, flag=F}}=Date,
    DComp= {{ {D},{T} }},
    G= ms_guards(F, DComp),
    MS= [{M,G,R}],



    %% @todo move to eunit
    %% TestFZ= #flatzone{tzname=Name
    %% 		     , wall_from=SDate, wall_to=SDate
    %% 		     , std_from=SDate, std_to=SDate
    %% 		     , utc_from=SDate, utc_to=SDate
    %% 		     },
    %% {ok, TestResult}= ets:test_ms(TestFZ, MS),
    %% error_logger:info_msg("TestFZ: ~p~n", [TestFZ]),
    %% error_logger:info_msg("MS: ~p~n", [MS]),
    %% error_logger:info_msg("TestResult: ~p~n", [TestResult]),

    MS.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flatten_all_zones(AllZones, AllRules) ->
    flatten_all_zones(AllZones, AllRules, []).

% recursively processes sets of similar zones until they're all done,
% passing zone sets to flatten_zone_set/1
flatten_all_zones([], _AllRules, FlatZones) ->
    FlatZones;
flatten_all_zones([Z1|_]= AllZones, AllRules, FlatZones) ->
    error_logger:info_msg("Flattening zones: ~s~n", [Z1#zone.name]),
    {CurrentZones, RestZones}= ezic_zone:split_by_name(Z1, AllZones),
    Flats= flatten_zone_set(CurrentZones, AllRules),
    flatten_all_zones(RestZones, AllRules, Flats ++ FlatZones).



contains_date2(#flatzone{utc_from=From, utc_to=To}, {Dt,#tztime{time=T, flag=F}})
  when F=:= u; F=:= g; F=:=z ->
    Date={Dt, T},
    ezic_date:compare(From, Date) andalso ezic_date:compare(Date, To);

contains_date2(#flatzone{std_from=From, std_to=To}, {Dt,#tztime{time=T, flag=s}})  ->
    Date={Dt, T},
    ezic_date:compare(From, Date) andalso ezic_date:compare(Date, To);

contains_date2(#flatzone{wall_from=From, wall_to=To}, {Dt,#tztime{time=T, flag=F}})
  when F=:=w; F=:=undefined ->
    Date={Dt, T},
    ezic_date:compare(From, Date) andalso ezic_date:compare(Date, To).




%% takes zones one-by-one from a list of zones of the same name.  It
%% gathers relevant rules and creates flat periods of the same gmt
%% offset (#flatzone). This is a recursive solution, eliminating Zones
%% from the list until it's been exhausted
flatten_zone_set(Zones, AllRules) ->
    flatten_zone_set(?MINFLAT, Zones, AllRules, [], none).


% flatten_zone_set(FromTime, Zones, AllRules, Flats) -> [#flatzone{}]
%    FromTime= #flatzone{*_from =/= undefined}
%    AllRules= [#rule{}]
%    Zones= [#zone{}]
%    Flats= [#flatzone{}]
%
% assumes a new zone every time it is called
flatten_zone_set(FromTimeStub=#flatzone{utc_from=UTCFrom, dstoffset=DSTOffset}
		 , Zones %[Z1=#zone{rule=RuleName, until=UntilTime, gmtoff=Offset} | _RestZones],
		 , AllRules
		 , Flats
		 , CurrentRule) ->

    [Zone | RestZones] = ezic_zone:next(Zones, UTCFrom, DSTOffset),
    #zone{rule=RuleName}= Zone,

    %% ?debugVal(Zone),

    %% we have a flatzone with start times;
    %% must populate the base gmt offset and name for the current zone

    FromTime= populate_flatzone(FromTimeStub, Zone),

    %% if this is the first run, DST offset is {0,0,0} if this is a
    %% recursion, DST offset is the previous zone's last DST offset

    %% @todo see if dst rules carry over zone changes IRL. This
    %% assumes they don't.

    %% gather all rules that _may_ apply

    Rules= [R || R <- AllRules, R#rule.name =:= RuleName ],

    %% ?debugVal(FromTime),
    %% ?debugVal(Rules),

    %% apply all rules in order, creating flatzones, until this zone
    %% ends, then regain control. rules have been exhausted, and zone
    %% is ending. let's finish this.

    {RuleFlats, NextFlat, EndingRule}= flatten_rule_set(FromTime, Zone, Rules, CurrentRule, []),
    FinalFlats= lists:append([RuleFlats, Flats]),

    %% ?debugVal(FinalFlats),
    %% ?debugVal(NextFlat),

    %% return flats if we've exceeded our years, or recurse if we can keep going
    NFUTCFrom = NextFlat#flatzone.utc_from,
    try maxyear_reached(NFUTCFrom) of
	true ->
	    %% ?debugMsg("maxyear reached from flatten_zone_set"),
	    FinalFlats;
	false ->
	    flatten_zone_set(NextFlat, RestZones, AllRules, FinalFlats, EndingRule)
    catch
	exit:Reason ->
	    %% ?debugMsg("bad year for nextflat:"),
	    %% ?debugVal(NextFlat),
	    erlang:error(Reason)
    end.





flatten_rule_set(FlatStart=#flatzone{utc_from=UTCFrom, dstoffset=DSTOffset, offset=Offset}
		 , Zone, Rules, CurrentRule, Flats) ->

    %% ?debugVal(Offset),
    %% ?debugVal(DSTOffset),
    %% ?debugVal(UTCFrom),

    ValidRules= lists:delete(CurrentRule, Rules),
    RulesWithDates= lists:foldl(
		 fun(R, Acc)->
			 case ezic_rule:project_next(R, Offset, DSTOffset, UTCFrom) of
			     none -> Acc;
			     {Year, D} -> [{D,Year, R} | Acc]
			 end
		 end
		 ,[], ValidRules),


    {UTCEndingRuleDate, EndingRuleYear, EndingRule}=
	case length(RulesWithDates) > 0 of
	    false -> {maximum, maximum, none};
	    true -> hd(lists:sort(RulesWithDates))
	end,

    ZoneDate= ezic_zone:project_end_utc(Zone, DSTOffset),


    %% ?debugVal(UTCEndingRuleDate),
    %% ?debugVal(EndingRuleYear),
    %% ?debugVal(EndingRule),
    %% ?debugVal(ZoneDate),


    try maxyear_reached(UTCEndingRuleDate) andalso maxyear_reached(ZoneDate) of
	true ->
	    %% ?debugMsg("maxyear reached from flatten_rule_set"),
	    {EndFlat, NextFlat}= finish_and_start_flat(max_year, FlatStart, DSTOffset),
	    NewFlats= [EndFlat | Flats],
	    {NewFlats, NextFlat, none};
	false ->

	    case ezic_date:compare(ZoneDate, UTCEndingRuleDate) of
		false ->
		    %% same zone, new rule
		    {EndFlat, NextFlat}= finish_and_start_flat(FlatStart, EndingRule, EndingRuleYear),
		    NewFlats= [EndFlat | Flats],
		    flatten_rule_set(NextFlat, Zone, Rules, EndingRule, NewFlats);
		true ->
						% zone change
		    case ezic_date:equal(ZoneDate, UTCEndingRuleDate) of
			false ->
						% zone only
			    {IEF, INF}= finish_and_start_flat(FlatStart, Zone, DSTOffset),
			    {[IEF | Flats], INF, CurrentRule};
			true ->
						% zone AND rule
			    {IEF, INF}= finish_flatzone_both(FlatStart, Zone, EndingRule, DSTOffset),
			    {[IEF | Flats], INF, EndingRule}
		    end
	    end
    catch
	error:{bad_year,Val} ->
	    error_logger:error_msg("bad year for EndingRule: ~p~n", [EndingRule]),
	    erlang:error(bad_year, Val)
    end.




%% rule is ending, while timezone remains
finish_and_start_flat(FlatStub=#flatzone{offset=Offset, dstoffset=OldDSTOffset}
		      , NewRule=#rule{save=NewDSTSave}
		      , EndingRuleYear) ->

    %% @todo for_rule_all was already called in a loop earlier. use those values instead.
    {{WD, WDn}, SD, UD}= ezic_date:for_rule(NewRule, Offset, OldDSTOffset, NewDSTSave, EndingRuleYear),
    {WDm, SDm, UDm}= ezic_date:m1s({WD, SD, UD}),

    EndFlat= ?ENDFLAT(FlatStub, WDm, SDm, UDm, OldDSTOffset),
    NewFlat1= ?FLAT(WDn, SD, UD),
    NewFlat2= NewFlat1#flatzone{offset=Offset, dstoffset=NewDSTSave, tzname=EndFlat#flatzone.tzname},

    FinalNewFlat= NewFlat2,

    %% ?debugVal(EndFlat),
    %% ?debugVal(FinalNewFlat),

    {EndFlat, FinalNewFlat};


%% timezone is ending, while rule remains the same.
%% returns the finished flatzone for the current zone, and a stub for
%% the next zone with the current DST offset and the UTC start
%% datetime
finish_and_start_flat(FlatStub=#flatzone{}, Zone=#zone{}, EndingDST) ->
    EndDatesP1={_,_,UD}= ezic_zone:project_end(Zone, EndingDST),
    {WDm, SDm, UDm}= ezic_date:m1s(EndDatesP1),
    EndFlat= ?ENDFLAT(FlatStub, WDm, SDm, UDm, EndingDST),

    RetNextFlat= #flatzone{dstoffset=EndingDST, utc_from=UD},

    %% ?debugVal(EndFlat),
%    ?debugVal(RetNextFlat),

    {EndFlat, RetNextFlat};

finish_and_start_flat(max_year, FlatStub, DSTOffset) ->
    EndFlat= ?ENDFLAT(FlatStub, current, current, current, DSTOffset),
    Stop= {{?MAXYEAR+1,1,1},{0,0,0}},
    NextFlat= #flatzone{utc_from=Stop},
    {EndFlat, NextFlat}.



%% both timezone and rule are ending at the same time
finish_flatzone_both(FlatStub=#flatzone{}, EndingZone=#zone{}, ChangingRule=#rule{save=NewDST}, EndingDST) ->
    %% ?debugVal(FlatStub),

    EndDatesP1={_,_,UD}= ezic_zone:project_end(EndingZone, EndingDST),
    {WDm, SDm, UDm}= ezic_date:m1s(EndDatesP1),
    EndFlat= ?ENDFLAT(FlatStub, WDm, SDm, UDm, EndingDST),

    %% ?debugVal(EndFlat),
    %% ?debugVal(ChangingRule),

    RetNextFlat= #flatzone{dstoffset=NewDST, utc_from=UD},
    {EndFlat, RetNextFlat}.


populate_flatzone(FZ=#flatzone{utc_from=UTCFrom, dstoffset=DSTOffset}
		  , #zone{name=Name, gmtoff=Offset}) ->
    UTCFromTZ= ezic_date:normalize(UTCFrom, u),
    {WT, ST, _}= ezic_date:all_times(UTCFromTZ, Offset, DSTOffset),
    FZ#flatzone{offset=Offset, tzname=Name, wall_from=WT, std_from=ST}.



maxyear_reached({{Y,_,_},_}) when Y > ?MAXYEAR ->
    true;
maxyear_reached({{Y,_,_},_}) when Y =< ?MAXYEAR ->
    false;
maxyear_reached(Atom) when Atom=:=maximum; Atom=:=current ->
    true; % for some N, taking n > N  =>  maximum > ?MAXYEAR
maxyear_reached(Val) ->
    erlang:error(bad_year, Val).





ms_guards(X, D) when X=:=u;X=:=g;X=:=z ->
    ms_guards2(D, '$5', '$6');
ms_guards(s, D) ->
    ms_guards2(D, '$3', '$4');
ms_guards(X, D) when X=:=w;X=:=undefined ->
    ms_guards2(D, '$1', '$2').


ms_guards2(D, From, To) ->
    [
     {'=<', From, D}
     , {'or', {'=<', D, To}, {'=:=', current, To}}
    ].
