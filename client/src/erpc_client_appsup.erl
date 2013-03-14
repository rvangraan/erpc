%%% File    : erpc_client_appsup.erl
%%% Description : 
%%% Copyright: (C) 2006 by Rudolph van Graan

-module(erpc_client_appsup).

-behaviour(supervisor).


%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API functions
%%====================================================================
start_link(Groupname,Backends) ->
  io:format("[ERPC] Starting client appsup for group ~p\n",[Groupname]),
  supervisor:start_link(?MODULE, [Groupname,Backends]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([Groupname,Backends]) ->
  io:format("[ERPC] Supervisor for group ~p started\n",[Groupname]),
  SupFlags = {one_for_one, 200, 600},
  ERPC = {erpc,{erpc,start_link,[Groupname,Backends]},
	  permanent,2000,worker,[erpc]},
  {ok,{SupFlags, [ERPC]}}.

%%====================================================================
%% Internal functions
%%====================================================================

