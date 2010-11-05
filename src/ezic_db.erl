
-module(ezic_db).
-include("include/ezic.hrl").
-include_lib("stdlib/include/qlc.hrl").


-export([zones/1, rules/1]).
-export([wipe/0, init/0, insert_all/1, getall/1]).


zones(Name) ->
    F = fun()->
		Q = qlc:q([Z || Z=#zone{name=N} <- mnesia:table(zone), N=:=Name]),
		qlc:e(Q)
	end,
    {atomic, Zones}= mnesia:transaction(F),
    Zones.
    
rules(Name) ->
    F = fun()->
		Q = qlc:q([R || R=#rule{name=N} <- mnesia:table(rule), N=:=Name]),
		qlc:e(Q)
	end,
    {atomic, Rules}= mnesia:transaction(F),
    Rules.
    






-define(create(Record),
	{atomic, ok} = mnesia:create_table(Record, [{type, bag}, {disc_copies, [node()]},{attributes, record_info(fields, Record)}])).


init() ->
    create_tabs(mnesia:create_schema([node()])),
    mnesia:wait_for_tables([rule, zone, link, leap], 3000).

wipe() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).


create_tabs(ok) ->
    mnesia:start(),

    ?create(rule),
    ?create(zone),
    ?create(link),
    ?create(leap),

    %% {atomic, ok}= mnesia:create_table(rule, [{disc_copies, [node()]},{attributes, record_info(fields, rule)}]),
    %% {atomic, ok}= mnesia:create_table(zone, [{disc_copies, [node()]},{attributes, record_info(fields, zone)}]),
    %% {atomic, ok}= mnesia:create_table(leap, [{disc_copies, [node()]},{attributes, record_info(fields, leap)}]),
    %% {atomic, ok}= mnesia:create_table(link, [{disc_copies, [node()]},{attributes, record_info(fields, link)}]);

    ok;
create_tabs({error, {_, {already_exists,_}}}) ->
    mnesia:start(),
    ok;
create_tabs(E) ->
    ?debug(E).


insert_all(Records) ->
    mnesia:transaction(
      fun() ->
	      lists:foreach(
		fun(R)-> mnesia:write(R) end,
		Records)
      end).
			       


getall(Tab) when is_atom(Tab) ->
    F = fun() ->
		Q = qlc:q([R || R<- mnesia:table(Tab)]),
		qlc:e(Q)
	end,
    mnesia:transaction(F).
    
