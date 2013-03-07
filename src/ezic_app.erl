%%%-------------------------------------------------------------------
%%% File    : ezic_app.erl
%%% Author  : aj <aj@fattie>
%%% Description :
%%%
%%% Created : 15 Nov 2010 by aj <aj@fattie>
%%%-------------------------------------------------------------------
-module(ezic_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------

%% TODO: remove by 2013.09.07 (6 months from today), to allow time for
%% the transition
start(_Type, [_]) ->
    erlang:error("Failed transition to v0.3.0: See Pull Request: https://github.com/drfloob/ezic/pull/15");
start(_Type, _StartArgs) ->
    {ok, DbMod} = application:get_env(db_mod),
    case ezic_sup:start_link(DbMod) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
