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
start_link(StartArgs) ->
    supervisor:start_link(?MODULE, StartArgs).

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
init(StartArgs) ->
    EzicDb = {ezic_db,{ezic_db, start_link, [StartArgs]},
	      permanent,2000,worker,[ezic_db]},
    {ok,{{one_for_all,3,30}, [EzicDb]}}.

%%====================================================================
%% Internal functions
%%====================================================================
