%%% File    : erpc_client_appsup.erl
%%% Description : 

-module(erpc_group_appsup).

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
  case application:get_env(erpc_client,groups) of
    {ok,[]} ->
      error_logger:warning_report(["ERPC SUPERVISOR - No ERPC groups defined",
				   {pid,self()},
				   {reason, "Group specification is missing - no groups started"}]),
      {ok,{SupFlags, []}};
    {ok,Groups} when is_list(Groups) ->
      Specs = lists:flatten([make_spec(Group) || Group <- Groups]),
      {ok,{SupFlags, Specs}};
    undefined -> 
      error_logger:warning_report(["ERPC SUPERVISOR - No ERPC groups defined",
				 {pid,self()},
				 {reason, "Group specification is missing - no groups started"}]),
      {ok,{SupFlags, []}}
  end.


make_spec({GroupName,Config}) when is_atom(GroupName),
				     is_list(Config) ->
  Backends = proplists:get_value(backends,Config,[]),
  {{erpc_group,GroupName},{erpc_client_appsup,start_link,[GroupName,Backends]},
   permanent,2000,supervisor,[]};
make_spec(InvalidSpec) ->
  error_logger:error_report(["ERPC SUPERVISOR - Group specification error",
			     {pid,self()},
			     {reason, "Group specification is invalid - group is not started"},
			     {spec,InvalidSpec}]),
  [].
  
  




%%====================================================================
%% Internal functions
%%====================================================================


