-module(ezic_db_ets_admin).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TABS, [rule, zone, leap, flatzone]).
-define(DB_FILENAME, "db.e2f").
-define(DETS_OPTS, []).
-define(ETS_OPTS, [named_table]).





-export([
	 init/0
	 , wipe/0
	 , insert_all/1
	 , wipe/1
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADMIN API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Entire DB
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


%% if dets tables exist,
%%   load stored tables
%% else
%%   create all ets tables as named tables
%%   load data
init() ->
    {ok, DbDir}= application:get_env(db_dir),
    Filename= filename:join(DbDir, ?DB_FILENAME),
    case db_sane(Filename) of 
	true -> load_tabfile(Filename);
	false -> create_tables(Filename)
    end,
    {ok, []}.


% erases all data
wipe() ->
    not_done.


% inserts all records into the appropriate db
insert_all(Record) ->
    not_done.



%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Single Table
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% erases date for one table
wipe(Tab) ->
    ezic_db_ets:wipe(Tab).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
    ets:insert(zone, Zones),

    ets:new(rule, [duplicate_bag, named_table]),
    ets:insert(rule, Rules),

    ets:new(flatzone, [duplicate_bag, named_table]),
    ezic_flatten:flatten(),

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



%% takes a populated dets table and converts it to an ets table.
ets_from_dets(Dets) ->
    Ets= ets:new(tzdb, ?ETS_OPTS),
    dets:to_ets(Dets, Ets).
