%%% File    : erpc_test_module.erl
%%% Author  : Ilse Potgieter <>
%%% Description : 
%%% Created : 30 Jan 2007 by Ilse Potgieter <>

-module(erpc_test_module).

-export([node/0,
	 exit/1,
	 throw_exception/2]).

node() ->
  erlang:node().

exit(Reason) ->
  erlang:exit(Reason).
  
throw_exception(Reason,Params) ->
  exception:throw(Reason,Params).
