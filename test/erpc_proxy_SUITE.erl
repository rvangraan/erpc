%%%-------------------------------------------------------------------
%%% File    : erpc_proxy_SUITE.erl
%%% Author  : Rudolph van Graan <>
%%% Description : 
%%%
%%% Created : 12 Nov 2006 by Rudolph van Graan <>
%%%-------------------------------------------------------------------
-module(erpc_proxy_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server/include/test_server.hrl").

%%--------------------------------------------------------------------
init_per_suite(Config) ->
  application:stop(erpc_srv),
  application:stop(erpc),
  Config.

%%--------------------------------------------------------------------
end_per_suite(_Config) ->
  application:start(erpc_srv),
  application:start(erpc),
  ok.

%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
  Config.

%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
  timer:sleep(500),
  ok.

%%--------------------------------------------------------------------

all(suite) -> 
  [start_no_connection,
   rpc_call_connection_not_ready,
   server_start,
   rpc_call,
   rpc_call_with_error,
   rpc_call_with_exit,
   rpc_call_with_throw,
   rpc_call_via_erpc_connection_in_error,
   test_invalid_module,
   create_stream,
   create_stream_sender,
   test_file_stream_set_cookie
  ].

%% Test cases starts here.
%%--------------------------------------------------------------------
start_no_connection(doc) -> 
  ["Start a connection when server is not running"];

start_no_connection(suite) -> 
  [];

start_no_connection(Config) when is_list(Config) -> 
  ?line {ok,PID} = erpc_connection:start_link(default,"localhost",55555),
  ?line {ok,error} = erpc_connection:status(PID),
  
  ?line {ok,_ServerPID} = erpc_srv:start_link(55555),
  F1 = fun() ->
	   timer:sleep(500),
	   ?line {ok,ready} = erpc_connection:status(PID)
       end,
  ?line {passed,{ok,ready}} = testhelper:try_n_times(5,F1),
  ?line Node = node(),
  ?line Node = erpc_connection:call(PID,erpc_test_module,node,[]),
  ok.

rpc_call_connection_not_ready(doc) ->
  ["Tests what happen when you attempt an rpc call on a connection in error state"];
rpc_call_connection_not_ready(suite) ->
  [];
rpc_call_connection_not_ready(Config) ->
  ?line {ok,PID} = erpc_connection:start_link(default,"localhost",55555),
  ?line {ok,error} = erpc_connection:status(PID),
  ?line {error,connection_not_ready} = erpc_connection:call(PID,erlang,node,[]),
  ok.
  
  
  

server_start(doc) ->
  ["Tests that erpc goes online as soon as a backend is available"];
server_start(suite) ->
  [];
server_start(_Config) ->
  process_flag(trap_exit,true),
  %% Start a server with only one backend connection
  ?line {ok,_StubPID} = erpc:start_link(default,[{"localhost",55555}]),
  ?line {ok,1} = erpc:connection_count(default),
  ?line {ok,no_backend} = erpc:status(default),
  ?line {error,no_backend} = erpc:get_ready_connection(default),

  ?line {ok,ServerPID} = erpc_srv:start_link(55555),
  F1 = fun() ->
	   timer:sleep(500),
	   {ok,ready} = erpc:status(default)
       end,
  %% Wait for connections to go online
  ?line {passed,{ok,ready}} = testhelper:try_n_times(5,F1),
  ?line {ok,_Connection} = erpc:get_ready_connection(default),

  %% Kill backend and see it go down
  ?line exit(ServerPID,kill),
  F2 = fun() ->
	   timer:sleep(500),
	   {ok,no_backend} = erpc:status(default)
       end,
  ?line {passed,{ok,no_backend}} = testhelper:try_n_times(5,F2),
  ok = stop_erpc().


rpc_call(doc) ->
  ["Tests that we can make an rpc call using a backend connection"];
rpc_call(suite) ->
  [];
rpc_call(_Config) ->
  process_flag(trap_exit,true),
  %% Start a server with only one backend connection
  ?line {ok,StubPID} = erpc:start_link(default,[{"localhost",55555}]),
  io:format("Stub = ~p\n",[StubPID]),
  ?line {ok,_ServerPID} = erpc_srv:start_link(55555),
  F1 = fun() ->
	   timer:sleep(500),
	   {ok,ready} = erpc:status(default)
       end,
  ?line {passed,{ok,ready}} = testhelper:try_n_times(5,F1),
  ?line Node = node(),
  ?line Node = erpc:call(default,erpc_test_module,node,[]),
  ok = stop_erpc().


rpc_call_with_error(doc) ->
  ["Tests rpc call that results in an error"];
rpc_call_with_error(suite) ->
  [];
rpc_call_with_error(_Config) ->
  process_flag(trap_exit,true),
  %% Start a server with only one backend connection
  ?line {ok,_StubPID} = erpc:start_link(default,[{"localhost",55555}]),
  ?line {ok,_ServerPID} = erpc_srv:start_link(55555),
  F1 = fun() ->
	   timer:sleep(500),
	   {ok,ready} = erpc:status(default)
       end,
  ?line {passed,{ok,ready}} = testhelper:try_n_times(5,F1),
  ?line Node = node(),
  try
    Node = erpc:call(default,erpc_test_module,a,[]),
    testhelper:fail("Test should have crashed!!!")
    catch
      error:E ->
	io:format("Error ~p\nCrash stack is ~p\n",[E,erlang:get_stacktrace()]),
	ok
    end,
  ok = stop_erpc().

rpc_call_with_throw(doc) ->
  ["Tests rpc call that results in a throw"];
rpc_call_with_throw(suite) ->
  [];
rpc_call_with_throw(_Config) ->
  process_flag(trap_exit,true),
  %% Start a server with only one backend connection
  ?line {ok,_StubPID} = erpc:start_link(default,[{"localhost",55555}]),
  ?line {ok,_ServerPID} = erpc_srv:start_link(55555),
  F1 = fun() ->
	   timer:sleep(500),
	   {ok,ready} = erpc:status(default)
       end,
  ?line {passed,{ok,ready}} = testhelper:try_n_times(5,F1),
  ?line Node = node(),
  try
    Node = erpc:call(default,erlang,throw,[hello]),
    testhelper:fail("Test should have crashed!!!")
    catch
      throw:E ->
	io:format("Throw ~p\nCrash stack is ~p\n",[E,erlang:get_stacktrace()]),
	ok
    end,
  ok = stop_erpc().


%%-----------------------------------------------------------------------------------------------------------

rpc_call_with_throw_exception(doc) ->
  ["Tests rpc call that results in a throw"];
rpc_call_with_throw_exception(suite) ->
  [];
rpc_call_with_throw_exception(_Config) ->
  process_flag(trap_exit,true),
  %% Start a server with only one backend connection
  ?line {ok,_StubPID} = erpc:start_link(default,[{"localhost",55555}]),
  ?line {ok,_ServerPID} = erpc_srv:start_link(55555),
  F1 = fun() ->
	   timer:sleep(500),
	   {ok,ready} = erpc:status(default)
       end,
  ?line {passed,{ok,ready}} = testhelper:try_n_times(5,F1),
  ?line Node = node(),
  try
    Node = erpc:call(default,erpc_test_module,throw_exception,[hello,[]]),
    testhelper:fail("Test should have crashed!!!")
    catch
      throw:E ->
	io:format("Throw ~p\nCrash stack is ~p\n",[E,erlang:get_stacktrace()]),
	ok
    end,
  ok = stop_erpc().

%%-----------------------------------------------------------------------------------------------------------

rpc_call_with_exit(doc) ->
  ["Tests rpc call that results in an exit"];
rpc_call_with_exit(suite) ->
  [];
rpc_call_with_exit(_Config) ->
  process_flag(trap_exit,true),
  %% Start a server with only one backend connection
  ?line {ok,_StubPID} = erpc:start_link(default,[{"localhost",55555}]),
  ?line {ok,_ServerPID} = erpc_srv:start_link(55555),
  F1 = fun() ->
	   timer:sleep(500),
	   {ok,ready} = erpc:status(default)
       end,
  ?line {passed,{ok,ready}} = testhelper:try_n_times(5,F1),
  ?line Node = node(),
  try
    Node = erpc:call(default,erpc_test_module,exit,[hello]),
    testhelper:fail("Test should have crashed!!!")
    catch
      exit:E ->
	io:format("Throw ~p\nCrash stack is ~p\n",[E,erlang:get_stacktrace()]),
	ok
    end,
  ok = stop_erpc().


rpc_call_via_erpc_connection_in_error(doc) ->
  ["Tests rpc call that results in an error"];
rpc_call_via_erpc_connection_in_error(suite) ->
  [];
rpc_call_via_erpc_connection_in_error(_Config) ->
  process_flag(trap_exit,true),
  %% Start a server with only one backend connection
  ?line {ok,_StubPID} = erpc:start_link(default,[]),
  {error,no_backend} = (catch erpc:call(default,erlang,node,[])),
  ok = stop_erpc().


test_invalid_module(doc) ->
  ["Tests rpc call with an invalid module (module not specified in system.config)"];
test_invalid_module(suite) ->
  [];
test_invalid_module(_Config) ->
  ?line {ok,PID} = erpc_connection:start_link(default,"localhost",55555),
  ?line {ok,error} = erpc_connection:status(PID),
  
  ?line {ok,_ServerPID} = erpc_srv:start_link(55555),
  F1 = fun() ->
	   timer:sleep(500),
	   ?line {ok,ready} = erpc_connection:status(PID)
       end,
  ?line {passed,{ok,ready}} = testhelper:try_n_times(5,F1),
  ?line {function_execution_denied,_} = (catch erpc_connection:call(PID,erlang,node,[])).


create_stream(doc) ->
  ["Tests that we can make an rpc call using a backend connection"];
create_stream(suite) ->
  [];
create_stream(_Config) ->
  process_flag(trap_exit,true),
  %% Start a server with only one backend connection
  ?line {ok,_StubPID} = erpc:start_link(default,[{"localhost",55555}]),
  ?line {ok,_ServerPID} = erpc_srv:start_link(55555),
  F1 = fun() ->
	   timer:sleep(500),
	   {ok,ready} = erpc:status(default)
       end,
  ?line {passed,{ok,ready}} = testhelper:try_n_times(5,F1),
  
  ModuleName = ?MODULE,
  InitParams = [self()],
  ?line {ok, StreamPID} = erpc:create_stream(default,ModuleName,InitParams),
  ?line StreamSendPID = erpc_stream_recv:stream_send_pid(StreamPID),
  try
    receive
      stream_init_called -> ok
    after
      2000 -> testhelper:fail("Should have received a message 1")
    end,
    ?line ok = erpc_stream_send:stream_call(StreamSendPID,packet_1),
    ?line ok = erpc_stream_send:stream_call(StreamSendPID,packet_2),
    ?line packet_1 = receive_from_stream(),
    ?line packet_2 = receive_from_stream()
    after
      MonitorRef = erlang:monitor(process,StreamPID),
      {ok,callback_closed} = erpc:close_stream(StreamPID),
      receive
	{'DOWN', MonitorRef, process, StreamPID, _Info} ->
	  ok
      after
	2000 -> testhelper:fail("Process did not die!")
      end
    end,
    ok = stop_erpc().


receive_from_stream() ->
  receive 
    {stream,Message} -> Message
  after
    2000 -> testhelper:fail("Should have received a message2")
  end.


create_stream_sender(doc) ->
  [];
create_stream_sender(suite) ->
  [];
create_stream_sender(_Config) ->
  process_flag(trap_exit,true),
  %% Start a server with only one backend connection
  ?line {ok,_StubPID} = erpc:start_link(default,[{"localhost",55555}]),
  ?line {ok,_ServerPID} = erpc_srv:start_link(55555),
  F1 = fun() ->
	   timer:sleep(500),
	   {ok,ready} = erpc:status(default)
       end,
  ?line {passed,{ok,ready}} = testhelper:try_n_times(5,F1),
  
  ?line {ok, RemoteStreamSendPID,_Connection} = erpc:create_remote_stream_sender(default,self()),
  Owner = self(),
  F2 = fun() ->
	   ok =  erpc_stream_send:stream_call(RemoteStreamSendPID,hello),
	   Owner ! function_returned,
	   normal
       end,
  proc_lib:spawn_link(F2),
  receive
    {'$gen_call',From,{stream_call,hello}} ->
      gen_server:reply(From,ok),
      receive function_returned -> ok end      
  after
    2000 -> testhelper:fail("No message")
  end,
  ok = stop_erpc(),

  timer:sleep(500).




test_file_stream_set_cookie(suite) ->
  [];
test_file_stream_set_cookie(Config) when is_list(Config) ->
  erlang:set_cookie(node(),'old-cookie'),
  ?line 'old-cookie' = erlang:get_cookie(),
  
  file:write_file("/tmp/invalid",<<"test">>),
  
  application:unset_env(erpc_srv,stream_cookies),
  
  erpc_file_stream:start(self(),node(),"/tmp/invalid",[]),
  ?line 'old-cookie' = erlang:get_cookie(),

  application:set_env(erpc_srv,stream_cookies,[{node(),'new-cookie'}]),
  
  erpc_file_stream:start(self(),node(),"/tmp/invalid",[]),
  ?line 'new-cookie' = erlang:get_cookie(),
  
  ok.





stop_erpc() ->
  case whereis(erpc) of
    undefined ->
      ok;
    PID ->
      Monitor = erlang:monitor(process,PID),
      exit(PID,shutdown),
      receive
	{'DOWN',Monitor,process,_Object,_Other} ->
	  ok
      end
  end.







-record(state,{owner_pid}).

%%=========================================================
%% Future Streaming Callback on the frontend
%%========================================================
erpc_stream_init(StreamPID,[OwnerPID] = _InitParams ) ->
  io:format("ERPC Stream Callback Init!\n"),
  OwnerPID ! stream_init_called,
  {ok,#state{owner_pid = OwnerPID}}.

erpc_stream_message(StreamPID,Message,State) ->
  io:format("ERPC Stream Message!\n"),
  OwnerPID = State#state.owner_pid,
  OwnerPID ! {stream,Message},
  %%Do stuff
  {ok,State}.

erpc_stream_closed(StreamPID,State) ->
  io:format("ERPC Stream Closed!\n"),
  {ok,callback_closed}.
%%=========================================================
%%=========================================================
