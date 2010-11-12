-module(ezic_flatten).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(FLAT(W,S,U), #flatzone{wall_from=W, std_from=S, utc_from=U}).
-define(ENDFLAT(F,W,S,U,D), F#flatzone{wall_to=W, std_to=S, utc_to=U, dstoffset=D}).
-define(MINFLAT, ?FLAT(minimum,minimum,minimum)).




-export([flatten/0]).




flatten() ->
    AllZones= ezic_db:get_all(zone),
    flatten_all_zones(AllZones),
    not_done.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% recursively processes sets of similar zones until they're all done,
% passing zone sets to flatten_zone_set/1
flatten_all_zones(Zones) ->
    flatten_all_zones(Zones, []).

flatten_all_zones([], Flats) ->
    Flats;
flatten_all_zones([Z1|_]= AllZones, Flats) ->
    {CurrentZones, RestZones}= ezic_zone:split_by_name(Z1, AllZones),
    NewFlats= lists:merge(Flats, flatten_zone_set(CurrentZones)),
    flatten_all_zones(RestZones, NewFlats).








%% takes zones one-by-one from a list of zones of the same name.  It
%% gathers relevant rules and creates flat periods of the same gmt
%% offset (#flatzone). This is a recursive solution, eliminating Zones
%% from the list until it's been exhausted
flatten_zone_set(Zones) ->
    flatten_zone_set(?MINFLAT, Zones, []).


% flatten_zone_set(FromTime, Zones, Flats) -> [#flatzone{}]
%    FromTime= #flatzone{*_from =/= undefined}
%    Zones= [#zone{}]
%    Flats= [#flatzone{}]
%
% assumes a new zone every time it is called
flatten_zone_set(_, [], Flats) ->
    erlang:error(debug_quit),
    Flats;
flatten_zone_set(FromTimeStub=#flatzone{utc_from=UTCFrom, dstoffset=DSTOffset}
		 , Zones %[Z1=#zone{rule=RuleName, until=UntilTime, gmtoff=Offset} | _RestZones], 
		 , Flats) ->

    [Zone | RestZones] = ezic_zone:next(Zones, UTCFrom, DSTOffset),
    #zone{rule=RuleName, until=_UntilTime, gmtoff=Offset}=Zone,

    %% we have a flatzone with start times; must populate the base gmt offset
    FromTime= FromTimeStub#flatzone{offset=Offset, tzname=Zone#zone.name},
    %% if this is the first run, DST offset is {0,0,0}
    %% if this is a recursion, DST offset is the previous zone's last DST offset
    

    %% we gather all rules that _may_ apply (same year)
    Rules= ezic_db:rules(RuleName),
    
    
    %% tack the date onto the zone, so we can see if the zone ends before a rule does
    TempZoneEndUTC= ezic_zone:project_end_utc(Zone, DSTOffset),
    ZoneWithDate= {TempZoneEndUTC, Zone},


    ?debugVal(FromTime),
    ?debugVal(ZoneWithDate),
    ?debugVal(Rules),

    {RuleFlats, LastFlat, EndingDST}= flatten_rule_set(FromTime, ZoneWithDate, Rules, []),
    ?debugVal(RuleFlats),
    ?debugVal(LastFlat),
    ?debugVal(EndingDST),
    
    %% rules have been exhausted, and zone is ending.
    {FinalFlat, NextFlat}= finish_and_start_flat(LastFlat, Zone, EndingDST),
    ?debugVal(FinalFlat),
    FinalFlats= lists:merge([[FinalFlat], RuleFlats, Flats]),
    ?debugVal(FinalFlats),

    
    flatten_zone_set(NextFlat, RestZones, FinalFlats).





flatten_rule_set(FlatStart=#flatzone{utc_from=UTCFrom, dstoffset=DSTOffset, offset=Offset}
		 , ZoneWithDate, Rules, Flats) ->
    
    %% add normalized (possibly inaccurate) dates for sorting
    %% purposes. note this may be empty. also note this MUST (I think)
    %% be done in the loop, since UTCFrom and DSTOffset can
    %% potentially change which rule comes next (however unlikely that
    %% case may be in real life)
    RulesWithDates= lists:foldl(
		 fun(R, Acc)-> 
			 case ezic_rule:project_next(R, Offset, DSTOffset, UTCFrom) of
			     none -> Acc;
			     D -> [{D,R} | Acc]
			 end
		 end
		 ,[], Rules),
    
    ?debugVal(RulesWithDates),

    {EndingRuleDate, EndingRule}= 
	case length(RulesWithDates) > 0 of
	    false -> {maximum, none};
	    true -> hd(lists:sort(RulesWithDates))
	end,

    {ZoneDate, _Zone}= ZoneWithDate,

    ?debugVal(EndingRuleDate),
    ?debugVal(ZoneDate),

    case ezic_date:compare(EndingRuleDate, ZoneDate) of
	true -> 
	    %% same zone, new rule
	    {EndFlat, NextFlat}= finish_and_start_flat(FlatStart, EndingRule, EndingRuleDate, Offset, DSTOffset),
	    NewFlats= [EndFlat | Flats],
	    flatten_rule_set(NextFlat, ZoneWithDate, Rules, NewFlats);
	false ->
	    %% new zone is handled in the caller: flatten_zone_set
	    {Flats, FlatStart, DSTOffset}
    end.





finish_and_start_flat(FlatStub=#flatzone{utc_from=_UTCFrom}, NewRule=#rule{save=NewDSTSave}, _EndingRuleDate={{ERDY,_,_},_}, Offset, DSTOffset) ->
    %% @todo for_rule_all was already called in a loop earlier. use those values instead.
    NewFlatStartDates={WD, SD, UD}=  ezic_date:for_rule_all(NewRule, Offset, DSTOffset, ERDY),
    {WDm, SDm, UDm}= ezic_date:m1s(NewFlatStartDates),

    EndFlat= ?ENDFLAT(FlatStub, WDm, SDm, UDm, DSTOffset),
    NewFlat1= ?FLAT(WD, SD, UD),
    NewFlat2= NewFlat1#flatzone{offset=Offset, dstoffset=NewDSTSave, tzname=EndFlat#flatzone.tzname},
    
    FinalNewFlat= NewFlat2,

    ?debugVal(EndFlat),
    ?debugVal(FinalNewFlat),

    {EndFlat, FinalNewFlat}.


finish_and_start_flat(FlatStub=#flatzone{}, Zone=#zone{}, EndingDST) ->
    EndDatesP1={WD,SD,UD}= ezic_zone:project_end(Zone, EndingDST),
    {WDm, SDm, UDm}= ezic_date:m1s(EndDatesP1),
    EndFlat= ?ENDFLAT(FlatStub, WDm, SDm, UDm, EndingDST),
    
    NextFlat1= ?FLAT(WD,SD,UD),
    NextFlat2= NextFlat1#flatzone{dstoffset=EndingDST},
    RetNextFlat= NextFlat2,

    ?debugVal(EndFlat),
    ?debugVal(RetNextFlat),

    {EndFlat, RetNextFlat}.
