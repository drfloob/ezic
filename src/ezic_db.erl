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


-record(state, {zones, rules, get_all, flatzone, insert_all, wipe, implementation}).

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

get_implementation() ->
    gen_server:call(?MODULE, {implementation}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN_SERVER 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_link(StartArgs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, StartArgs, []).

init(ezic_db_mnesia) ->
    ezic_db_mnesia:init(),
    State = #state{zones = fun ezic_db_mnesia:zones/1,
		   rules = fun ezic_db_mnesia:rules/1,
		   get_all = fun ezic_db_mnesia:get_all/1,
		   flatzone = fun ezic_db_mnesia:flatzone/2,
		   insert_all = fun ezic_db_mnesia:insert_all/1,
		   wipe = fun ezic_db_mnesia:wipe/1,
		   implementation = "mnesia"},
    {ok, State};
init(_) ->
    ezic_db_ets:init(),
    State = #state{zones = fun ezic_db_ets:zones/1,
		   rules = fun ezic_db_ets:rules/1,
		   get_all = fun ezic_db_ets:get_all/1,
		   flatzone = fun ezic_db_ets:flatzone/2,
		   insert_all = fun ezic_db_ets:insert_all/1,
		   wipe = fun ezic_db_ets:wipe/1,
		   implementation = "ets"},
    {ok, State}.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% DB Lookup
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


handle_call({zones, Name}, _, State) ->
    ZoneFun= State#state.zones,
    Matches= ZoneFun(Name),
    {reply, Matches, State};
handle_call({rules, Name}, _, State) ->
    RuleFun= State#state.rules,
    Matches= RuleFun(Name),
    {reply, Matches, State};
handle_call({all, Tab}, _, State) ->
    GetAllFun= State#state.get_all,
    Matches= GetAllFun(Tab),
    {reply, Matches, State};
handle_call({flatzone, Date, Name}, _, State) ->
    FlatzoneFun= State#state.flatzone,
    Result= FlatzoneFun(Date, Name),
    {reply, Result, State};
handle_call({insert_all, Records}, _, State) ->
    InsertAllFun= State#state.insert_all,
    InsertAllFun(Records),
    {noreply, State};
handle_call({wipe, Tab}, _, State) ->
    WipeFun= State#state.wipe,
    Result= WipeFun(Tab),
    {reply, Result, State};
handle_call({flatten}, _, State) ->
    GetAllFun= State#state.get_all,
    Zones= GetAllFun(zone),
    Rules= GetAllFun(rule),
    FlatZone= ezic_flatten:flatten(Zones, Rules),
    InsertAllFun= State#state.insert_all,
    Result= InsertAllFun(FlatZone),
    {reply, Result, State};
handle_call({implementation}, _, State) ->
    {reply, State#state.implementation, State};
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
