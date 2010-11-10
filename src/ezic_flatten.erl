-module(ezic_flatten).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(FLAT(W,S,U), #flatzone{wall_from=W, std_from=S, utc_from=U}).
-define(ENDFLAT(F,W,S,U), F#flatzone{wall_to=W, std_to=S, utc_to=U}).
-define(MINFLAT, ?FLAT(minimum,minimum,minimum)).




-export([flatten/0]).




flatten() ->
    AllZones= ezic_db:get_all(zone),
    flatten_zones(AllZones),
    not_done.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% recursively processes sets of similar zones until they're all done, passing zone sets to flatten_zone_set/1
flatten_zones(Zones) ->
    flatten_zones(Zones, []).

flatten_zones([], Flats) ->
    Flats;
flatten_zones([Z1|_]= AllZones, Flats) ->
    {CurrentZones, RestZones}= ezic_zone:split_by_name(Z1, AllZones),
    NewFlats= lists:merge(Flats, flatten_zone_set(CurrentZones)),
    flatten_zones(RestZones, NewFlats).








% takes zones one-by-one, gathering relevant rules and creating flat periods of the same gmt offset (#flatzone)
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
		 , Flats) ->

    {Zone, RestZones}= ezic_zone:next(Zones, UTCFrom, DSTOffset),
    #zone{rule=RuleName, until=UntilTime, gmtoff=Offset}=Zone,

    %% we have a flatzone with start times, we populate the base offset
    FromTime= FromTimeStub#flatzone{offset=Offset},
    %% note that dst offset default to {0,0,0} in #flatzone{}
    

    %% we gather all rules that _may_ apply (same year)
    Rules= ezic_db:rules(RuleName),
    
    %% and add normalized (inaccurate) dates for sorting purposes
    %% note this may be empty
    RulesWithDates= lists:foldl(
		 fun(R, Acc)-> 
			 case ezic_rule:project_next(R, Offset, DSTOffset, UTCFrom) of
			     none -> Acc;
			     D -> [{D,R} | Acc]
			 end
		 end
		 ,[], Rules),
    %% tack on the zone w/ date, in case it ends first
    ZoneWithDate= {ezic_zone:project_end_utc(Zone, DSTOffset), Zone},

    [NextEventWithDate | RestWithDates]= lists:sort(RulesAndZoneWithDates),
    ?debugVal(NextEventWithDate),
    ?debugVal(RestWithDates),

    {_,NextEvent}= NextEventWithDate,
    {EndFlat, NextFlat}= finish_and_start_flat(FromTime, NextEvent, Offset, DSTOffset),
    NewFlats= [EndFlat | Flats],
    
    RestZones= [Z || {_,Z} <- RestWithDates],

    flatten_zone_set(NextFlat, RestZones, NewFlats).






finish_and_start_flat(Flat=#flatzone{}, Zone=#zone{}, _Offset, DSTOffset) ->
    NextStart= {WT, ST, UT}= ezic_zone:project_end(Zone, DSTOffset),
    {MWT, MST, MUT}= ezic_date:m1s(NextStart),
    EndFlat= ?ENDFLAT(Flat, MWT, MST, MUT),
    NextFlat= ?FLAT(WT, ST, UT),
    {EndFlat, NextFlat};

finish_and_start_flat(Flat=#flatzone{utc_from=UTCFrom}, Rule=#rule{}, Offset, DSTOffset) ->
    not_done.

