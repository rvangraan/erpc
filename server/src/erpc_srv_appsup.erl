%%% File    : erpc_srv_appsup.erl
%%% Author  : Ilse Potgieter <>
%%% Description : 
%%% Created : 13 Nov 2006 by Ilse Potgieter <>

-module(erpc_srv_appsup).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% Application callbacks
%%====================================================================
start(_Type, Args) ->
  case start_link(Args) of
    {ok, Pid} -> 
      {ok, Pid};
    Error ->
      Error
  end.

stop(_State) ->
  ok.


%%====================================================================
%% API functions
%%====================================================================
start_link(Args) ->
  supervisor:start_link(?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
  SupFlags = {one_for_one, 200, 600},
  {ok,Port} = application:get_env(erpc_srv,server_port),
  ERPC  = {erpc_srv,{erpc_srv,start_link,[Port]},
	      permanent,2000,worker,[erpc_srv]},
  {ok,{SupFlags, [ERPC]}}.

%%====================================================================
%% Internal functions
%%====================================================================

