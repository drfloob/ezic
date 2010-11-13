-module(ezic_flatten).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(FLAT(W,S,U), #flatzone{wall_from=W, std_from=S, utc_from=U}).
-define(ENDFLAT(F,W,S,U,D), F#flatzone{wall_to=W, std_to=S, utc_to=U, dstoffset=D}).
-define(MINFLAT, ?FLAT(minimum,minimum,minimum)).



%% @todo move MAXYEAR to config file
-define(MAXYEAR, 2500). % last year to process flatzones for.


-export([flatten/0]).


%% debug
-export([flatten_all_zones/1]).



flatten() ->
    AllZones= ezic_db:get_all(zone),
    flatten_all_zones(AllZones),
    done.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% recursively processes sets of similar zones until they're all done,
% passing zone sets to flatten_zone_set/1
flatten_all_zones([]) ->
    done;
flatten_all_zones([Z1|_]= AllZones) ->
    {CurrentZones, RestZones}= ezic_zone:split_by_name(Z1, AllZones),

    Flats= flatten_zone_set(CurrentZones),
    ezic_db:insert_all(Flats),

    flatten_all_zones(RestZones).








%% takes zones one-by-one from a list of zones of the same name.  It
%% gathers relevant rules and creates flat periods of the same gmt
%% offset (#flatzone). This is a recursive solution, eliminating Zones
%% from the list until it's been exhausted
flatten_zone_set(Zones) ->
    flatten_zone_set(?MINFLAT, Zones, [], none).


% flatten_zone_set(FromTime, Zones, Flats) -> [#flatzone{}]
%    FromTime= #flatzone{*_from =/= undefined}
%    Zones= [#zone{}]
%    Flats= [#flatzone{}]
%
% assumes a new zone every time it is called
flatten_zone_set(FromTimeStub=#flatzone{utc_from=UTCFrom, dstoffset=DSTOffset}
		 , Zones %[Z1=#zone{rule=RuleName, until=UntilTime, gmtoff=Offset} | _RestZones], 
		 , Flats
		 , CurrentRule) ->

    [Zone | RestZones] = ezic_zone:next(Zones, UTCFrom, DSTOffset),
    #zone{rule=RuleName, until=_UntilTime, gmtoff=Offset}= Zone,

    ?debugVal(Zone),

    %% we have a flatzone with start times; 
    %% must populate the base gmt offset and name for the current zone

    FromTime= populate_flatzone(FromTimeStub, Zone),

    %% if this is the first run, DST offset is {0,0,0} if this is a
    %% recursion, DST offset is the previous zone's last DST offset
    %% @todo see if dst rules carry over zone changes IRL. I'll assume
    %% they don't.
    

    %% we gather all rules that _may_ apply
    Rules= ezic_db:rules(RuleName),
    
    
    ?debugVal(FromTime),
    ?debugVal(Rules),


    %% apply all rules in order, creating flatzones, until this zone
    %% ends, then regain control. rules have been exhausted, and zone
    %% is ending. let's finish this.

    {RuleFlats, NextFlat, EndingRule}= flatten_rule_set(FromTime, Zone, Rules, CurrentRule, []),
    FinalFlats= lists:append([RuleFlats, Flats]),

    ?debugVal(FinalFlats),
    ?debugVal(NextFlat),
    
    %% return flats if we've exceeded our years, or recurse if we can keep going
    NFUTCFrom = NextFlat#flatzone.utc_from,
    case maxyear_reached(NFUTCFrom) of
	true -> 
	    ?debugMsg("maxyear reached from flatten_zone_set"),
	    FinalFlats;
	false -> 
	    flatten_zone_set(NextFlat, RestZones, FinalFlats, EndingRule)
    end.





flatten_rule_set(FlatStart=#flatzone{utc_from=UTCFrom, dstoffset=DSTOffset, offset=Offset}
		 , Zone, Rules, CurrentRule, Flats) ->

    
    ValidRules= lists:delete(CurrentRule, Rules),
    RulesWithDates= lists:foldl(
		 fun(R, Acc)-> 
			 case ezic_rule:project_next(R, Offset, DSTOffset, UTCFrom) of
			     none -> Acc;
			     D -> [{D,R} | Acc]
			 end
		 end
		 ,[], ValidRules),
    

    {UTCEndingRuleDate, EndingRule}= 
	case length(RulesWithDates) > 0 of
	    false -> {maximum, none};
	    true -> hd(lists:sort(RulesWithDates))
	end,
    
    ZoneDate= ezic_zone:project_end_utc(Zone, DSTOffset),


    ?debugVal(UTCEndingRuleDate),
    ?debugVal(ZoneDate),

    
    case maxyear_reached(UTCEndingRuleDate) andalso maxyear_reached(ZoneDate) of
	true -> 
	    ?debugMsg("maxyear reached from flatten_rule_set"),
	    {Flats, #flatzone{utc_from=maximum}, none};
	false ->

	    case ezic_date:compare(ZoneDate, UTCEndingRuleDate) of
		false -> 
		    %% same zone, new rule
		    {EndFlat, NextFlat}= finish_and_start_flat(FlatStart, EndingRule, UTCEndingRuleDate),
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
    end.




%% rule is ending, while timezone remains
finish_and_start_flat(FlatStub=#flatzone{offset=Offset, dstoffset=OldDSTOffset}
		      , NewRule=#rule{save=NewDSTSave}
		      , _EndingRuleDate={{ERDY,_,_},_}) ->
    
    %% @todo for_rule_all was already called in a loop earlier. use those values instead.
    {{WD, WDn}, SD, UD}= ezic_date:for_rule(NewRule, Offset, OldDSTOffset, NewDSTSave, ERDY),
    {WDm, SDm, UDm}= ezic_date:m1s({WD, SD, UD}),

    EndFlat= ?ENDFLAT(FlatStub, WDm, SDm, UDm, OldDSTOffset),
    NewFlat1= ?FLAT(WDn, SD, UD),
    NewFlat2= NewFlat1#flatzone{offset=Offset, dstoffset=NewDSTSave, tzname=EndFlat#flatzone.tzname},
    
    FinalNewFlat= NewFlat2,

    ?debugVal(EndFlat),
    ?debugVal(FinalNewFlat),

    {EndFlat, FinalNewFlat};


%% timezone is ending, while rule remains the same.
%% returns the finished flatzone for the current zone, and a stub for
%% the next zone with the current DST offset and the UTC start
%% datetime
finish_and_start_flat(FlatStub=#flatzone{}, Zone=#zone{}, EndingDST) ->
    EndDatesP1={WD,SD,UD}= ezic_zone:project_end(Zone, EndingDST),
    {WDm, SDm, UDm}= ezic_date:m1s(EndDatesP1),
    EndFlat= ?ENDFLAT(FlatStub, WDm, SDm, UDm, EndingDST),
    
    RetNextFlat= #flatzone{dstoffset=EndingDST, utc_from=UD},

    ?debugVal(EndFlat),
%    ?debugVal(RetNextFlat),

    {EndFlat, RetNextFlat}.


%% both timezone and rule are ending at the same time
finish_flatzone_both(FlatStub=#flatzone{}, EndingZone=#zone{}, ChangingRule=#rule{save=NewDST}, EndingDST) ->
    ?debugVal(FlatStub),

    EndDatesP1={WD,SD,UD}= ezic_zone:project_end(EndingZone, EndingDST),
    {WDm, SDm, UDm}= ezic_date:m1s(EndDatesP1),
    EndFlat= ?ENDFLAT(FlatStub, WDm, SDm, UDm, EndingDST),

    ?debugVal(EndFlat),
    ?debugVal(ChangingRule),

    RetNextFlat= #flatzone{dstoffset=NewDST, utc_from=UD},
    {EndFlat, RetNextFlat}.


populate_flatzone(
  FZ=#flatzone{utc_from=UTCFrom, dstoffset=DSTOffset}
  , Zone=#zone{name=Name, gmtoff=Offset}) ->

    UTCFromTZ= ezic_date:normalize(UTCFrom, u),
    {WT, ST, UTCFRom}= ezic_date:all_times(UTCFromTZ, Offset, DSTOffset),
    FZ#flatzone{offset=Offset, tzname=Name, wall_from=WT, std_from=ST}.



maxyear_reached({{Y,_,_},_}) when Y > ?MAXYEAR ->
    true;
maxyear_reached({{Y,_,_},_}) when Y =< ?MAXYEAR ->
    false;
maxyear_reached(Atom) when Atom=:=maximum; Atom=:=current ->
    true; % for some N, taking n > N  =>  maximum > ?MAXYEAR
maxyear_reached(Val) ->
    erlang:error(bad_year, Val).
