-module(ezic_compile).
-include("include/ezic.hrl").

-export([flatten/2, separate/1, zone_sorter/2]).



% @todo figure out what to do with leap seconds




separate(Records) ->
    Zones= [Z || Z=#zone{} <- Records],
    Rules= [Z || Z=#rule{} <- Records],
    Leaps= [Z || Z=#leap{} <- Records],
    Links= [Z || Z=#link{} <- Records],
    {ok, Zones, Rules, Leaps, Links}.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FLATTENING PSEUDOCODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% args: Zones, Rules
% 
% take the first zone, get all zones with the same name
% sort them by ascending date (oldest first)
% foreach zone:
%   find all rules between start (if exists) & end (if exists) of zone
%   foreach rule:
%     create a #flatzone
% flatten the rest of the zones
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


flatten(Zones, Rules) ->
%    ?debug("Rules: ~p", [Rules]),
    flatten(Zones, Rules, []).

% note: recursive
flatten([], _, Flats) ->
    Flats;
flatten(Zones, Rules, Flats) ->
    [Zone | _] = Zones,
%    ?debug("Zone: ~p", [Zone]),
    ?debug("Flattening: ~p", [Zone#zone.name]),

    {ZoneSet, RestZones} = lists:splitwith(make_zone_filter(Zone), Zones),
%    ?debug("ZoneSet: ~p", [ZoneSet]),
    

    NeededRules= lists:filter(make_rule_zone_filter(ZoneSet), Rules),
%    ?debug("NeededRules: ~p", [NeededRules]),

    SortedZoneSet= lists:sort(fun zone_sorter/2, ZoneSet),
    
    NewFlats= lists:append(flatten_set(SortedZoneSet, NeededRules), Flats),
%    ?debug("Sorted Zones: ~p", [SortedZoneSet]),

    flatten(RestZones, Rules, NewFlats).




zone_sorter(#zone{until=U1}, #zone{until=U2}) ->
    ezic_date:compare_datetimes(U1, U2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% filter generators
make_zone_filter(#zone{name=Name}) ->
    fun(#zone{name=N}) when Name=:=N-> true; (_)->false end.


make_rule_zone_filter(ZoneSet) ->
    ZS= sets:from_list([Z#zone.rule || Z<-ZoneSet]),
%    ?debug("~n\t ZoneSetRules: ~p", [sets:to_list(ZS)]),
    fun(#rule{name=R})->
	    sets:is_element(R, ZS)
    end.


% accepts a rule, and returns true if it exists between the two years given
make_rule_time_filter(D1, D2) ->
    fun(#rule{from=F, to=T}) ->
	    ezic_date:overlap({D1,D2}, {F,T})
       end.



flatten_set(ZoneSet, Rules) ->
    flatten_set(ZoneSet, Rules, none, []).

% note: recursive
flatten_set([], _, _, Flats) ->
    Flats;
flatten_set(ZoneSet, Rules, Prev, Flats) ->
    [Zone|Rest] = ZoneSet,
    
    NewFlats= lists:append(flatten_one(Zone, Prev, Rules), Flats),
    flatten_set(Rest, Rules, Zone, NewFlats).



flatten_one(Zone=#zone{until=Until}, none, Rules) ->
    NeededRules= lists:filter(make_rule_time_filter(minimum, Until), Rules),
    lists:map(fun(R)-> flatten_each(Zone, R, minimum) end, NeededRules);
flatten_one(Zone=#zone{until=UTo}, NextZone=#zone{until=UFrom}, Rules) ->
    NeededRules= lists:filter(make_rule_time_filter(UFrom, UTo), Rules),
    lists:map(fun(R)-> flatten_each(Zone, R, UFrom) end, NeededRules).


flatten_each(Zone, Rule, From) ->
%    ?debug("flatten_each: ~n\tZone: ~p~n\tRule: ~p~n\tFrom: ~p", [Zone, Rule, From]),
%    erlang:error(dbg),
    not_done.
    



next_zone([]) ->
    none;
next_zone([H|T]) ->
    H.


