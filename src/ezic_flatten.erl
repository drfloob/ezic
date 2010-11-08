-module(ezic_flatten).
-include("include/ezic.hrl").

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



make_next_flat(#flatzone{wall_to=W, std_to=S, utc_to=U, offset=O}) ->
    make_next_flat({W,S,U}, O);
make_next_flat(EndTimes) ->
    make_next_flat(EndTimes, undefined).


make_next_flat({W,S,U}, O) ->
    FW= ezic_date:add_seconds(W,1),
    FS= ezic_date:add_seconds(S,1),
    FU= ezic_date:add_seconds(U,1),
    Flat= ?FLAT(FW, FS, FU),
    Flat#flatzone{offset=O}.

    
    
end_flat(Flat=#flatzone{}, {W,S,U}) ->
    ?ENDFLAT(Flat,W,S,U).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% recursively processes sets of similar zones until they're all done, passing zone sets to flatten_zone_set/1
flatten_zones(Zones) ->
    flatten_zones(Zones, []).

flatten_zones([], Flats) ->
    Flats;
flatten_zones([Z1|_]= AllZones, Flats) ->
    {CurrentZones, RestZones}= ezic_zone:split_by_name(Z1, AllZones),

    SortedZones= lists:sort(fun ezic_zone:sort_ascending/2, CurrentZones),
    NewFlats= lists:merge(Flats, flatten_zone_set(SortedZones)),

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
flatten_zone_set(FromTimeStub, [Z1=#zone{rule=RuleName, until=UntilTime, gmtoff=Offset} | _RestZones] = _Zones, _Flats) ->

    %% we have a flatzone with start times, we populate the base offset
    FromTime= FromTimeStub#flatzone{offset=Offset},
    

    %% we gather all rules that _may_ apply (same year)
    Rules= ezic_db:rules(RuleName),
    RelevantRules= ezic_rule:filter(FromTime, UntilTime, Rules),
    SortedRules= lists:sort(fun ezic_rule:sort_ascending/2, RelevantRules),


    %% Then we plot points, bouncing from rule to rule, ending when the zone ends
    {ok, _NextFromTimeStub, _NewFlats}= flatten_zone(FromTime, Z1, SortedRules),
    
    
    not_done.










% flattens a single zone into multiple flatzones.
% FromTime has its *_from fields and offset field populated. (it should anyway)
% @todo invalid input checking
% Rules are sorted
flatten_zone(BaseFlat, Zone, Rules) ->
    flatten_zone(BaseFlat, Zone, Rules, []).
    
flatten_zone(BaseFlat, Zone=#zone{until=Until, gmtoff=Offset}, Rules, Flats) ->
    case ezic_rule:next_event(BaseFlat, Until, Offset, Rules) of
         % is this enough information to compare all times corectly?
	{end_until, EndTimes} ->
	    NewFlats= [end_flat(BaseFlat, EndTimes) | Flats],
	    NewFrom= make_next_flat(EndTimes),
	    {ok, NewFrom, NewFlats}; % base case
	
	{end_rule, EndTimes, NextOffset} ->
	    NewFlats= [end_flat(BaseFlat, EndTimes) | Flats],
	    NewFrom= make_next_flat(EndTimes, NextOffset),
	    flatten_zone(NewFrom, Zone, Rules, NewFlats) % tail recursion

	end.




