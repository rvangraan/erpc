%%% File    : erpc_call_skeleton.erl
%%% Author  : Rudolph van Graan <>
%%% Description : Implements the actual remote call
%%% Created : 11 Nov 2006 by Rudolph van Graan <>
%%% Copyright: (C) 2006,2007 by Rudolph van Graan

-module(erpc_call_skeleton).

-export([go/5]).

-include("../../include/erpc.hrl"). 

go(Connection,OrigFrom,M,F,A) ->
  Result = 
    try
      ok = authenticate_module(M),
      R = apply(M,F,A),
      {result,R}
    catch
      throw:Exception when is_record(Exception,exception) -> {throw,fix_exception_stack(Exception,M,F,A)};
      throw:Term                                          -> {throw,Term};
      exit:Reason                                         -> {exit,Reason};
      error:Reason                                        -> {error,Reason,erlang:get_stacktrace()}
    end,
  complete(Connection,OrigFrom,Result).


complete(Connection,OrigFrom,Result) ->
  erpc_connection_endpoint:complete(Connection,OrigFrom,Result).
  
authenticate_module(Module) ->	
  AllowedModules = 
  case application:get_env(erpc_srv,allowed_modules) of
     undefined    -> [];
     {ok,Modules} -> Modules
   end,
  case lists:member(Module,AllowedModules) of
    true  -> ok;
    false -> throw({function_execution_denied,Module})
  end.
      
fix_exception_stack(Exception,M,F,A) ->
  Stack = Exception#exception.stack,
  Index =
  try
    customlists:indexof({erpc_call_skeleton,go,5},Stack) - 1
  catch
    throw:{not_found,_} -> length(Stack)
  end,
  {StackHead,StackTail} = lists:split(Index,Stack),
  Exception#exception{ stack = StackHead ++ [{M,F,length(A),A}|StackTail] }.