-module(ezic_flatten).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(FLAT(W,S,U), #flatzone{wall_from=W, std_from=S, utc_from=U}).
-define(ENDFLAT(F,W,S,U,D), F#flatzone{wall_to=W, std_to=S, utc_to=U, dstoffset=D}).
-define(MINFLAT, ?FLAT(minimum,minimum,minimum)).




-export([flatten/0]).




flatten() ->
    AllZones= ezic_db:get_all(zone),
    flatten_zones(AllZones),
    not_done.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% recursively processes sets of similar zones until they're all done,
% passing zone sets to flatten_zone_set/1
flatten_zones(Zones) ->
    flatten_zones(Zones, []).

flatten_zones([], Flats) ->
    Flats;
flatten_zones([Z1|_]= AllZones, Flats) ->
    {CurrentZones, RestZones}= ezic_zone:split_by_name(Z1, AllZones),
    NewFlats= lists:merge(Flats, flatten_zone_set(CurrentZones)),
    flatten_zones(RestZones, NewFlats).








% takes zones one-by-one, gathering relevant rules and creating flat
% periods of the same gmt offset (#flatzone). This is a recursive
% solution, eliminating Zones until they've been exhausted
flatten_zone_set(Zones) ->
    flatten_zone_set(?MINFLAT, Zones, []).


% flatten_zone_set(FromTime, Zones, Flats) -> [#flatzone{}]
%    FromTime= #flatzone{*_from =/= undefined}
%    Zones= [#zone{}]
%    Flats= [#flatzone{}]
%
% assumes a new zone every time it is called
flatten_zone_set(_, [], Flats) ->
      Flats;
flatten_zone_set(FromTimeStub=#flatzone{utc_from=UTCFrom, dstoffset=DSTOffset}
		 , Zones %[Z1=#zone{rule=RuleName, until=UntilTime, gmtoff=Offset} | _RestZones], 
		 , _Flats) ->

    [Zone | _RestZones] = ezic_zone:next(Zones, UTCFrom, DSTOffset),
    #zone{rule=RuleName, until=_UntilTime, gmtoff=Offset}=Zone,

    %% we have a flatzone with start times, we populate the base offset
    FromTime= FromTimeStub#flatzone{offset=Offset},
    %% note that dst offset default to {0,0,0} in #flatzone{}
    

    %% we gather all rules that _may_ apply (same year)
    Rules= ezic_db:rules(RuleName),
    
    
    %% tack the date onto the zone, so we can see if it ends before a rule does
    ZoneWithDate= {ezic_zone:project_end_utc(Zone, DSTOffset), Zone},


    ?debugVal(FromTime),
    ?debugVal(ZoneWithDate),
    ?debugVal(Rules),

    RuleFlats= flatten_rule_set(FromTime, ZoneWithDate, Rules, []),
    ?debugVal(RuleFlats),

    %% %% and add normalized (possibly inaccurate) dates for sorting purposes
    %% %% note this may be empty
    %% RulesWithDates= lists:foldl(
    %% 		 fun(R, Acc)-> 
    %% 			 case ezic_rule:project_next(R, Offset, DSTOffset, UTCFrom) of
    %% 			     none -> Acc;
    %% 			     D -> [{D,R} | Acc]
    %% 			 end
    %% 		 end
    %% 		 ,[], Rules),

    %% [NextEventWithDate | RestWithDates]= lists:sort(RulesAndZoneWithDates),

    %% {_,NextEvent}= NextEventWithDate,
    %% {EndFlat, NextFlat}= finish_and_start_flat(FromTime, NextEvent, Offset, DSTOffset),
    %% NewFlats= [EndFlat | Flats],
    
    %% RestZones= [Z || {_,Z} <- RestWithDates],

    %% flatten_zone_set(NextFlat, RestZones, NewFlats).
    
    not_done.





flatten_rule_set(FlatStart=#flatzone{utc_from=UTCFrom, dstoffset=DSTOffset, offset=Offset}
		 , ZoneWithDate, Rules, Flats) ->
    
    %% and add normalized (possibly inaccurate) dates for sorting purposes
    %% note this may be empty
    %% also note this MUST (I think) be done in the loop, since UTCFrom and DSTOffset 
    %%  can potentially change which rule comes next (however unlikely)
    RulesWithDates= lists:foldl(
		 fun(R, Acc)-> 
			 case ezic_rule:project_next(R, Offset, DSTOffset, UTCFrom) of
			     none -> Acc;
			     D -> [{D,R} | Acc]
			 end
		 end
		 ,[], Rules),
    
    ?debugVal(RulesWithDates),

    [{EndingRuleDate, EndingRule} | _]= lists:sort(RulesWithDates),
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
	    %% new zone is handled in calling function: flatten_zone_set
	    Flats
    end.





finish_and_start_flat(FlatStub=#flatzone{utc_from=_UTCFrom}, EndingRule=#rule{}, _EndingRuleDate={{ERDY,_,_},_}, Offset, DSTOffset) ->
    NewFlatStartDates={WD, SD, UD}=  ezic_date:for_rule_all(EndingRule, Offset, DSTOffset, ERDY),
    _FlatEndDates={WDm, SDm, UDm}= ezic_date:m1s(NewFlatStartDates),

    EndFlat= ?ENDFLAT(FlatStub, WDm, SDm, UDm, DSTOffset),
    NewFlat1= ?FLAT(WD, SD, UD),
    NewFlat2= NewFlat1#flatzone{offset=Offset},
    
    FinalNewFlat= NewFlat2,
    {EndFlat, FinalNewFlat}.



%% finish_and_start_flat(Flat=#flatzone{}, Zone=#zone{}, _Offset, DSTOffset) ->
%%     NextStart= {WT, ST, UT}= ezic_zone:project_end(Zone, DSTOffset),
%%     {MWT, MST, MUT}= ezic_date:m1s(NextStart),
%%     EndFlat= ?ENDFLAT(Flat, MWT, MST, MUT),
%%     NextFlat= ?FLAT(WT, ST, UT),
%%     {EndFlat, NextFlat};


