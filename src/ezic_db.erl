-module(ezic_db).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).


-export([
	 zones/1
	 , rules/1
	 , flatzones/1
	 , flatzone/2
	 %, insert/2
	 , get_all/1
	 , insert_all/1
	 , wipe/1
	 , flatten/0
	 , get_implementation/0
	]).

-export([
	 start_link/1
	 , init/1
	 , code_change/3
	 , handle_call/3
	 , handle_cast/2
	 , handle_info/2
	 , terminate/2
	 ]).


-record(state, {mod}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


zones(TzName) ->
    gen_server:call(?MODULE, {zones, TzName}).


rules(TzName) ->
    gen_server:call(?MODULE, {rules, TzName}).


flatzones(TzName) ->
    gen_server:call(?MODULE, {flatzones, TzName}).


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

get_implementation() ->
    gen_server:call(?MODULE, {implementation}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN_SERVER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_link(StartArgs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, StartArgs, []).


%% TODO: create a db behavior
init(DbModule) when DbModule =:= ezic_db_mnesia;
		    DbModule=:= ezic_db_ets ->
    DbModule:init(),
    State = #state{mod = DbModule},
    {ok, State}.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% DB Lookup
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


handle_call({zones, Name}, _, State) ->
    Mod= State#state.mod,
    Matches= Mod:zones(Name),
    {reply, Matches, State};
handle_call({rules, Name}, _, State) ->
    Mod= State#state.mod,
    Matches= Mod:rules(Name),
    {reply, Matches, State};
handle_call({all, Tab}, _, State) ->
    Mod= State#state.mod,
    Matches= Mod:get_all(Tab),
    {reply, Matches, State};
handle_call({flatzones, Name}, _, State) ->
    Mod= State#state.mod,
    Result= Mod:flatzones(Name),
    {reply, Result, State};
handle_call({flatzone, Date, Name}, _, State) ->
    Mod= State#state.mod,
    Result= Mod:flatzone(Date, Name),
    {reply, Result, State};
handle_call({insert_all, Records}, _, State) ->
    Mod= State#state.mod,
    Mod:insert_all(Records),
    {noreply, State};
handle_call({wipe, Tab}, _, State) ->
    Mod= State#state.mod,
    Result= Mod:wipe(Tab),
    {reply, Result, State};
handle_call({flatten}, _, State) ->
    Mod= State#state.mod,
    Zones= Mod:get_all(zone),
    Rules= Mod:get_all(rule),
    FlatZone= ezic_flatten:flatten(Zones, Rules),
    Result= Mod:insert_all(FlatZone),
    {reply, Result, State};
handle_call({implementation}, _, State) ->
    Mod= State#state.mod,
    Impl= Mod:implementation(),
    {reply, Impl, State};
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
