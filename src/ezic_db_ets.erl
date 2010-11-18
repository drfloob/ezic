-module(ezic_db_ets).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).


-export([
	 zones/1
	 , rules/1
	 , flatzone/2
	 , all/1
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


handle_call({zones, Name}, _, Ets) ->
    Matches= ets:select(Ets, [{#zone{name=Name, _='_'}, [], ['$_']}]),
    {reply, Matches, Ets};
handle_call({rules, Name}, _, Ets) ->
    Matches= ets:select(Ets, [{#rule{name=Name, _='_'}, [], ['$_']}]),
    {reply, Matches, Ets};
handle_call({all, Table}, _, Ets) ->
    Matches= ets:lookup(Ets, Table),
    {reply, Matches, Ets};
handle_call({flatzone, Date, Name}, _, Ets) ->
    Matches= ets:select(Ets, ezic_flatten:ms(Date, Name)),
    {reply, Matches, Ets};
handle_call(_, _, Ets) ->
    {noreply, Ets}.


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
