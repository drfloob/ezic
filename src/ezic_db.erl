-module(ezic_db).
-include("include/ezic.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([zones/1, rules/1, flatzones/1, flatzone/2]).
-export([wipe/0, wipe/1, init/0, insert_all/1, get_all/1]).


-define(create(Record),
	{atomic, ok} = mnesia:create_table(Record, 
			   [{type, bag}
			    , {disc_copies, [node()]}
			    , {attributes, record_info(fields, Record)}
			   ])).



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
    
% get all records from table
get_all(Tab) when is_atom(Tab) ->
    F = fun() ->
		Q = qlc:q([R || R<- mnesia:table(Tab)]),
		qlc:e(Q)
	end,
    {atomic, Ret}= mnesia:transaction(F),
    Ret.


flatzones(TzName) ->
    F = fun() ->
		Q = qlc:q([Fz || Fz=#flatzone{tzname=N}<- mnesia:table(flatzone), N=:=TzName]),
		qlc:e(Q)
	end,
    {atomic, Ret}= mnesia:transaction(F),
    Ret.
    

flatzone(Date, TzName) ->
    F = fun()->
		Q = qlc:q(
		      [Fz || Fz=#flatzone{tzname=N} <- mnesia:table(flatzone)
				 , N=:=TzName
				 , ezic_flatten:contains_date(Fz, Date)]),
		qlc:e(Q)
	end,
    {atomic, [FlatZone]}= mnesia:transaction(F),
    FlatZone.
    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WRITE - insertion/edit methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



insert_all(Records) ->
    mnesia:transaction(
      fun() ->
	      lists:foreach(
		fun(R)-> mnesia:write(R) end,
		Records)
      end).
			       



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADMIN - initialization and administration methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% initialize the db
init() ->
    create_tabs(mnesia:create_schema([node()])),
    mnesia:wait_for_tables([rule, zone, link, leap, flatzone], 3000).


% WARNING: deletes all db files
wipe() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).



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

    ok;
create_tabs({error, {_, {already_exists,_}}}) ->
    mnesia:start(),
    ok;
create_tabs(E) ->
    ?debugVal(E).




