%%%-------------------------------------------------------------------
%%% File    : erpc_tcp_SUITE.erl
%%% Author  : Rudolph van Graan <>
%%% Description : 
%%%
%%% Created : 11 Nov 2006 by Rudolph van Graan <>
%%%-------------------------------------------------------------------
-module(erpc_tcp_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server/include/test_server.hrl").

%%--------------------------------------------------------------------
init_per_suite(Config) ->
  application:stop(erpc_srv),
  application:stop(erpc),
  {ok,ServerPID} = erpc_srv:start(11005),
  params:replace([{server,ServerPID}],Config).

%%--------------------------------------------------------------------
end_per_suite(Config) ->
  ServerPID = params:fget(server,Config),
  ok = erpc_srv:stop(),
  application:start(erpc_srv),
  application:start(erpc),
  params:delete([server],Config).

%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
  {ok,Conn} = gen_tcp:connect("localhost",11005,[binary,{packet,4},{active,false}]),
  params:replace([{conn,Conn}],Config).
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
  Conn      = params:fget(conn,Config),  
  ok = gen_tcp:close(Conn),
  params:delete([conn],Config).

%%--------------------------------------------------------------------
all(doc) -> 
  ["Describe the main purpose of this suite"];

all(suite) -> 
  [ basic_call,
   call_with_exit,
   call_with_error,
   echo].

%% Test cases starts here.
%%--------------------------------------------------------------------
basic_call(suite) -> 
  [];

basic_call(Config) when is_list(Config) -> 
  Conn = params:fget(conn,Config),
  From = blah,
  ?line Request = {rpc,From,erpc_test_module,node,[]},
  ?line Packet = term_to_binary(Request),
  ?line ok = gen_tcp:send(Conn,Packet),
  ?line {ok,ReplyPacket} = gen_tcp:recv(Conn,0),
  ?line Node = node(),
  ?line {rpc_result,From,{result,Node}} = binary_to_term(ReplyPacket),
  ok.

  
call_with_exit(suite) ->
  [];
call_with_exit(Config) ->
  Conn = params:fget(conn,Config),
  From = blah,
  ?line Request = {rpc,From,erpc_test_module,exit,[sommer]},
  ?line Packet = term_to_binary(Request),
  ?line ok = gen_tcp:send(Conn,Packet),
  ?line {ok,ReplyPacket} = gen_tcp:recv(Conn,0),
  ?line {rpc_result,From,{exit,sommer}} = binary_to_term(ReplyPacket),
  ok.


call_with_error(suite) ->
  [];
call_with_error(Config) ->
  Conn = params:fget(conn,Config),
  From = blah,
  ?line Request = {rpc,From,erpc_test_module,a,[sommer]},
  ?line Packet = term_to_binary(Request),
  ?line ok = gen_tcp:send(Conn,Packet),
  ?line {ok,ReplyPacket} = gen_tcp:recv(Conn,0),
  ?line {rpc_result,From,{error,undef,Stack}} = binary_to_term(ReplyPacket),
  ok.


echo(suite) ->
  [];
echo(Config) ->
  Conn = params:fget(conn,Config),
  ?line Request = {echo,1},
  ?line Packet = term_to_binary(Request),
  ?line ok = gen_tcp:send(Conn,Packet),
  ?line {ok,ReplyPacket} = gen_tcp:recv(Conn,0),
  ?line {echo_response,1} = binary_to_term(ReplyPacket),
  ok.
