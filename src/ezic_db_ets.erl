-module(ezic_db_ets).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DB_FILENAME, "db.e2f").

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


implementation() -> ?MODULE.

zones(TzName) ->
    ets:select(zone, [{#zone{name=TzName, _='_'}, [], ['$_']}]).

rules(TzName) ->
    ets:select(rule, [{#rule{name=TzName, _='_'}, [], ['$_']}]).

flatzones(TzName) ->
    ets:select(flatzone, [{#flatzone{tzname=TzName, _='_'}, [], ['$_']}]).


flatzone(Date, TzName) ->
    FlatZones= ets:select(flatzone, ezic_flatten:ms(Date, TzName)),
    case length(FlatZones) of
	1 ->
	    hd(FlatZones);
	2 ->
	    {error, {ambiguous_zone, FlatZones}};
	0 ->
	    {error, no_zone};
	_ ->
	    {error, {should_not_happen, {FlatZones, Date, TzName}}}
    end.


get_all(Tab) ->
    try ets:lookup(Tab, Tab) of X -> X
    catch error:X -> {error, X}
    end.




insert_all(Records) ->
    Zones= [ZI || ZI <- Records, ZI = #zone{}],
    ets:insert(zone, Zones),
    Rules= [RI || RI <- Records, RI = #rule{}],
    ets:insert(rule, Rules),
    FlatZones= [FZ || FZ <- Records, FZ = #flatzone{}],
    ets:insert(flatzone, FlatZones).


%wipe() ->
%    gen_server:call(?MODULE, {wipe}).


wipe(Tab) ->
    ets:delete(Tab, Tab).


init() ->
    {ok, DbDir}= application:get_env(db_dir),
    Filename= filename:join(DbDir, ?DB_FILENAME),
    case db_sane(Filename) of
	true -> load_tabfile(Filename);
	false -> create_tables(Filename)
    end,
    {ok, []}.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Private functions for initializing ets
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


%% -> true if table exists
db_sane(Filename) ->
    %% @todo ensure data is present for each record type.

    case ets:tabfile_info(Filename) of
	{ok, _} -> true;
	{error, _} -> false
    end.



%% creates the dets table and populates it.
create_tables(Filename) ->
    {ok, Zones, Rules, _, _} = ezic_record:separate(ezic_loader:load()),

    ets:new(zone, [duplicate_bag, named_table]),
    true = ets:insert(zone, Zones),

    ets:new(rule, [duplicate_bag, named_table]),
    true = ets:insert(rule, Rules),

    ets:new(flatzone, [duplicate_bag, named_table]),
    %% error_logger:info_msg("~p~n", [Rules]),
    FlatZones = ezic_flatten:flatten(Zones, Rules),
    true = ets:insert(flatzone, FlatZones),

    % combine into one ets
    Ets= ets:new(ezic_db_ets, [duplicate_bag]),
    ets:insert(Ets, ets:lookup(zone, zone)),
    ets:insert(Ets, ets:lookup(rule, rule)),
    ets:insert(Ets, ets:lookup(flatzone, flatzone)),

    % save to disk
    ets:tab2file(Ets, Filename),

    ets:delete(Ets),

    ok.



%% loads or creates the dets table.
load_tabfile(Filename) ->
    {ok, Ets}= ets:file2tab(Filename),

    Zones= ets:lookup(Ets, zone),
    ets:new(zone, [duplicate_bag, named_table]),
    ets:insert(zone, Zones),

    Rules= ets:lookup(Ets, rule),
    ets:new(rule, [duplicate_bag, named_table]),
    ets:insert(rule, Rules),

    FlatZones= ets:lookup(Ets, flatzone),
    ets:new(flatzone, [duplicate_bag, named_table]),
    ets:insert(flatzone, FlatZones),

    ets:delete(Ets),
    ok.
