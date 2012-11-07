-module(ezic_db_ets).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).


-export([
	 zones/1
	 , rules/1
	 , flatzone/2
	 , all/1
	 %, insert_all/1
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


all(Tab) ->
    gen_server:call(?MODULE, {all, Tab}).


%insert_all(Records) ->
%    gen_server:call(?MODULE, {insert_all, Records}).


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
    ezic_db_ets_admin:init().

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% DB Lookup
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


handle_call({zones, Name}, _, State) ->
    Matches= ets:select(zone, [{#zone{name=Name, _='_'}, [], ['$_']}]),
    {reply, Matches, State};
handle_call({rules, Name}, _, State) ->
    Matches= ets:select(rule, [{#rule{name=Name, _='_'}, [], ['$_']}]),
    {reply, Matches, State};
handle_call({all, Tab}, _, State) ->
    Matches= ets:lookup(Tab, Tab),
    {reply, Matches, State};
handle_call({flatzone, Date, Name}, _, State) ->
    FlatZones= ets:select(flatzone, ezic_flatten:ms(Date, Name)),
    Result= case length(FlatZones) of
		1 ->
		    hd(FlatZones);
		2 ->
		    erlang:error(ambiguous_zone, FlatZones);
		0 ->
		    erlang:error(no_zone);
		_ ->
		    erlang:error(should_not_happen, {FlatZones, Date, Name})
	    end,
    {reply, Result, State};
%handle_call({insert_all, Records}, _, Ets) ->
%    Result= ets:insert(Ets, Records),
%    {reply, Result, Ets};
%handle_call({wipe}, _, Ets) ->
%    Result= ets:delete(Ets),
%    {reply, Result, Ets};
handle_call({wipe, Tab}, _, State) ->
    Result= ets:delete(Tab, Tab),
    {reply, Result, State};
handle_call({flatten}, _, State) ->
    Result= ezic_flatten:flatten(),
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

terminate(_, State) ->
    ok.
