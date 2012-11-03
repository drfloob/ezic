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
    Ets= case db_sane(Filename) of 
	true -> load_tabfile(Filename);
	false -> create_tables(Filename)
    end,
    {ok, Ets}.


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
    Ets= ets:new(ezic_ets_db, [duplicate_bag]),
    ets:insert(Ets, ezic_loader:load()),
    ezic_flatten:flatten(Ets),

    % save to disk
    ets:tab2file(Ets, Filename),

    Ets.



%% loads or creates the dets table.
load_tabfile(Filename) ->
    {ok, Name}= ets:file2tab(Filename),
    Name.



%% takes a populated dets table and converts it to an ets table.
ets_from_dets(Dets) ->
    Ets= ets:new(tzdb, ?ETS_OPTS),
    dets:to_ets(Dets, Ets).
