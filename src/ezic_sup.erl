%%%-------------------------------------------------------------------
%%% File    : ezic_sup.erl
%%% Author  : aj <aj@fattie>
%%% Description : 
%%%
%%% Created : 15 Nov 2010 by aj <aj@fattie>
%%%-------------------------------------------------------------------
-module(ezic_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(_) ->
    supervisor:start_link(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init(_) ->
    EzicDb = {ezic_db,{ezic_db, start_link, []},
	      permanent,2000,worker,[ezic_db_ets]},
    {ok,{{one_for_all,3,30}, [EzicDb]}}.

%%====================================================================
%% Internal functions
%%====================================================================
