-module(ezic_db_mnesia).
-include("include/ezic.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([
	 init/0
	 , zones/1
	 , rules/1
	 , flatzones/1
	 , flatzone/2
	 %, insert/2
	 , get_all/1
	 , insert_all/1
	 , wipe/1
	 , implementation/0
	]).

-define(create(Record),
	{atomic, ok} = mnesia:create_table(Record,
			   [{type, bag}
			    , {disc_copies, [node()]}
			    , {attributes, record_info(fields, Record)}
			   ])).


implementation() -> ?MODULE.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% READ - db reading methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% retrieve all zones by name
zones(Name) ->
    F = fun()->
		Q = qlc:q([Z || Z=#zone{name=N} <- mnesia:table(zone), N=:=Name]),
		qlc:e(Q)
	end,
    {atomic, Zones}= mnesia:transaction(F),
    Zones.

% retrieve all rules by name
rules(Name) ->
    F = fun()->
		Q = qlc:q([R || R=#rule{name=N} <- mnesia:table(rule), N=:=Name]),
		qlc:e(Q)
	end,
    {atomic, Rules}= mnesia:transaction(F),
    Rules.

% retrieve all flatzones by tz name
flatzones(Name) ->
    F = fun()->
		Q = qlc:q([Z || Z=#flatzone{tzname=N} <- mnesia:table(flatzone), N=:=Name]),
		qlc:e(Q)
	end,
    {atomic, FlatZones}= mnesia:transaction(F),
    FlatZones.

% get all records from table
get_all(Tab) when is_atom(Tab) ->
    F = fun() ->
		Q = qlc:q([R || R<- mnesia:table(Tab)]),
		qlc:e(Q)
	end,
    {atomic, Ret}= mnesia:transaction(F),
    Ret.

%% get a flatzone for a specific date
%% Date :: {date(), #tztime{}}
%% returns #flatzone{}
%%  or throws either error:
%%    * {ambiguous_zone, [Z1,Z2,...]}
%%    * {no_zone}
flatzone(Date, TzName) ->
    %% @todo validate Date
    F = fun()->
		Q = qlc:q(
		      [Fz || Fz=#flatzone{tzname=N} <- mnesia:table(flatzone)
				 , N=:=TzName
				 , ezic_flatten:contains_date(Fz, Date)]),
		qlc:e(Q)
	end,
    {atomic, FlatZones}= mnesia:transaction(F),

    case length(FlatZones) of
	1 ->
	    hd(FlatZones);
	2 ->
	    erlang:error(ambiguous_zone, FlatZones);
	0 ->
	    erlang:error(no_zone);
	_ ->
	    erlang:error(should_not_happen, {FlatZones, Date, TzName})
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADMIN - initialization and administration methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% initialize the db
init() ->
    create_tabs(mnesia:create_schema([node()])),
    mnesia:wait_for_tables([rule, zone, link, leap, flatzone], 3000).


% WARNING: deletes all db files
%wipe() ->
%    mnesia:stop(),
%    mnesia:delete_schema([node()]).



% WARNING: deletes all entries in table Tab
wipe(Tab) ->
    mnesia:transaction(
      fun()->
	      mnesia:delete(Tab, '_', write)
      end).



create_tabs(ok) ->
    mnesia:start(),

    ?create(rule),
    ?create(zone),
    ?create(link),
    ?create(leap),
    ?create(flatzone),

    {ok, Zones, Rules, _, _} = ezic_record:separate(ezic_loader:load()),
    insert_all(Zones),
    insert_all(Rules),
    FlatZones = ezic_flatten:flatten(Zones, Rules),
    error_logger:info_msg("~p~n", [FlatZones]),
    insert_all(FlatZones),

    ok;
create_tabs({error, {_, {already_exists,_}}}) ->
    mnesia:start(),
    ok;
create_tabs(E) ->
    ?debugVal(E).


insert_all(Records) ->
    mnesia:transaction(
      fun() ->
	      lists:foreach(
		fun(R)-> mnesia:write(R) end,
		Records)
      end).
