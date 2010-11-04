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
% sort them by descending date (newest first)
% foreach zone:
%   find all rules between start & end(if exists) of zone
%   foreach rule:
%     create a #flatzone
% flatten the rest of the zones
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% note: recursive
flatten([], Rules) ->
    done;
flatten(Zones, Rules) ->
    [Zone | _] = Zones,
%    ?debug("Zone: ~p", [Zone]),
    ?debug("Flattening: ~p", [Zone#zone.name]),

    {ZoneSet, RestZones} = lists:splitwith(make_zone_filter(Zone), Zones),
%    ?debug("ZoneSet: ~p", [ZoneSet]),
    

    NeededRules= lists:takewhile(make_rule_zone_filter(Zone), Rules),
%    ?debug("NeededRules: ~p", [NeededRules]),

    SortedZoneSet= lists:sort(fun zone_sorter/2, ZoneSet),
    
    flatten_set(SortedZoneSet, NeededRules),
%    ?debug("Sorted Zones: ~p", [SortedZoneSet]),

    flatten(RestZones, Rules).




zone_sorter(Z1=#zone{until=U1},#zone{until=U2}) ->
    ezic_date:compare_datetimes(U1, U2).


    %% case (D1 =< D2) of
    %% 	true ->
    %% 	    true;
    %% 	false ->
    %% 	    ezic_record:compare_time(T1, T2)
    %% end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% filter generators
make_zone_filter(#zone{name=Name}) ->
    fun(#zone{name=N}) when Name=:=N-> true; (_)->false end.


make_rule_zone_filter(#zone{rule=Rule}) ->
    fun(#rule{name=Rule})-> true; (_)->false end.


% accepts a rule, and returns true if it exists between the two years given
make_rule_time_filter(D1, D2) ->
    fun(#rule{from=F, to=T}) ->
	    ezic_date:overlap({D1,D2}, {F,T})
       end.



% note: recursive
flatten_set([], Rules) ->
    done;
flatten_set(ZoneSet, Rules) ->
    [Zone|Rest] = ZoneSet,
    NextZone= next_zone(Rest),
    
    flatten_one(Zone, NextZone, Rules),
    flatten_set(Rest, Rules).



flatten_one(Zone=#zone{until=Until}, none, Rules) ->
    NeededRules= lists:takewhile(make_rule_time_filter({0,0,0}, Until), Rules),
    lists:foreach(fun(R)-> flatten_each(Zone, R) end, Rules);
flatten_one(Zone=#zone{until=UTo}, NextZone=#zone{until=UFrom}, Rules) ->
    NeededRules= lists:takewhile(make_rule_time_filter(UFrom, UTo), Rules),
    lists:foreach(fun(R)-> flatten_each(Zone, R) end, Rules).


flatten_each(Zone, Rule) ->
    not_done.
    



next_zone([]) ->
    none;
next_zone([H|T]) ->
    H.


