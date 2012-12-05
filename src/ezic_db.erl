-module(ezic_db).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).


-export([
	 zones/1
	 , rules/1
	 , flatzone/2
	 %, insert/2
	 , get_all/1
	 , insert_all/1
	 , wipe/1
	 , flatten/0
	]).

-export([
	 start_link/0
	 , init/1
	 , code_change/3
	 , handle_call/3
	 , handle_cast/2
	 , handle_info/2
	 , terminate/2
	 ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


zones(TzName) ->
    gen_server:call(?MODULE, {zones, TzName}).


rules(TzName) ->
    gen_server:call(?MODULE, {rules, TzName}).


flatzone(Date, TzName) ->
    gen_server:call(?MODULE, {flatzone, Date, TzName}).


get_all(Tab) ->
    gen_server:call(?MODULE, {all, Tab}).


insert_all(Records) ->
    gen_server:call(?MODULE, {insert_all, Records}).


%wipe() ->
%    gen_server:call(?MODULE, {wipe}).


wipe(Tab) ->
    gen_server:call(?MODULE, {wipe, Tab}).


flatten() ->
    gen_server:call(?MODULE, {flatten}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN_SERVER 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ezic_db_ets:init(),
    {ok, []}.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% DB Lookup
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


handle_call({zones, Name}, _, State) ->
    Matches= ezic_db_ets:zones(Name),
    {reply, Matches, State};
handle_call({rules, Name}, _, State) ->
    Matches= ezic_db_ets:rules(Name),
    {reply, Matches, State};
handle_call({all, Tab}, _, State) ->
    Matches= ezic_db_ets:get_all(Tab),
    {reply, Matches, State};
handle_call({flatzone, Date, Name}, _, State) ->
    Result= ezic_db_ets:flatzone(Date, Name),
    {reply, Result, State};
handle_call({insert_all, Records}, _, State) ->
    ezic_db_ets:insert_all(Records),
    {noreply, State};
handle_call({wipe, Tab}, _, State) ->
    Result= ezic_db_ets:wipe(Tab),
    {reply, Result, State};
handle_call({flatten}, _, State) ->
    Zones= ezic_db_ets:get_all(zone),
    Rules= ezic_db_ets:get_all(rule),
    FlatZone= ezic_flatten:flatten(Zones, Rules),
    Result= ezic_db_ets:insert_all(FlatZone),
    {reply, Result, State};
handle_call(_, _, State) ->
    {noreply, State}.


%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Other gen_server
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


code_change(_, State, _) ->
    {ok, State}.


handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.
