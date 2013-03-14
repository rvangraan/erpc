%%%-------------------------------------------------------------------
%%% File    : erpc_connection.erl
%%% Author  : Rudolph van Graan <>
%%% Description : 
%%%
%%% Created : 12 Nov 2006 by Rudolph van Graan <>
%%%-------------------------------------------------------------------
-module(erpc_connection).

-behaviour(gen_fsm).

%% API
-export([start_link/3,
	 status/1,
	 create_remote_stream_sender/2,
	 deallocate_remote_stream_sender/2,
	 call/4]).

%% gen_fsm callbacks
-export([ready/3,
	 error/3]).

-export([init/1, 
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([run/2,
	 go/2]).

%%--------------------------------------------------------------------
-include("common/utils/include/exception.hrl").
%%--------------------------------------------------------------------


-record(state, {address,
	        port,
	        socket,
		heartbeat,
		timer, 
		groupname,
		log_flag = false,
	        ttl = 3}).

%%--------------------------------------------------------------------
start_link(Groupname,Address,Port) ->
  R = gen_fsm:start_link(?MODULE, [Groupname,Address,Port], []),
  R.


status(Connection) ->
  gen_fsm:sync_send_all_state_event(Connection,status,infinity).

call(Connection,M,F,A) ->
  R = try
	gen_fsm:sync_send_event(Connection,{call,M,F,A},infinity)
      catch
	exit:{noproc,E} ->
	  {error,connection_not_ready}
      end,
  case R  of
    {result,Result}              -> Result;
    {throw,Exception} when is_record(Exception,exception) -> throw(Exception);
    {throw,Reason}               -> throw(Reason);
    {exit,Reason}                -> exit(Reason);
    {error,Reason,Stack}         -> erlang:error({rpc_failed,[{reason,Reason},{mfa,M,F,A},{stack,Stack}]});
    {error,connection_not_ready} -> {error,connection_not_ready}
  end.
    

create_remote_stream_sender(Connection,StreamRecvPID) ->
  gen_fsm:sync_send_event(Connection,{create_stream_sender,StreamRecvPID},infinity).
    
  
deallocate_remote_stream_sender(Connection,RemoteStreamSendPID) ->
  gen_fsm:sync_send_event(Connection,{deallocate_remote_stream_sender,RemoteStreamSendPID},infinity).
  

%%--------------------------------------------------------------------
init([Groupname,Address,Port]) ->
  process_flag(trap_exit,true),
  PID = start_heartbeat(1000),
  error_logger:info_report([{subsystem,"ERPC"},
			    {description,"ERPC Connection started"},
			    {pid,self()},
			    {address,Address},
			    {port,Port}]),
  {ok,error, #state{address = Address, 
		    port = Port,
		    groupname = Groupname,
		    heartbeat = PID}}.


%%--------------------------------------------------------------------
ready({call,M,F,A}, From, State) ->
  Request = {rpc,From,M,F,A},
  Packet = term_to_binary(Request),
  ok = gen_tcp:send(State#state.socket,Packet),
  {next_state, ready, State};
ready({create_stream_sender,StreamRecvPID}, From, State) ->
  Request = {create_stream_sender,From,StreamRecvPID},
  Packet = term_to_binary(Request),
  ok = gen_tcp:send(State#state.socket,Packet),
  {next_state, ready, State};
ready({deallocate_remote_stream_sender,RemoteStreamSendPID},_From,State) ->
  Request = {deallocate_remote_stream_sender,RemoteStreamSendPID},
  Packet = term_to_binary(Request),
  ok = gen_tcp:send(State#state.socket,Packet),
  {reply,ok, ready, State};
ready(heartbeat,_From,State) ->
  {reply,ok, ready, State}.

error(heartbeat, _From, State)->
%  io:format("ERPC: Connecting to ~s:~p\n",[State#state.address,State#state.port]),
  case gen_tcp:connect(State#state.address,State#state.port,[binary,{packet,4},{active,true}]) of
    {ok,Socket} ->
      error_logger:info_report([{subsystem,"ERPC"},
				{description,"ERPC Connection - connection open"},
				{pid,self()},
				{address,State#state.address},
				{port,State#state.port}]),

%      io:format("ERPC: Connected to ~s:~p\n",[State#state.address,State#state.port]),
      %% RvG - don't check return as tests may run this connection without erpc
      erpc:add_ready_connection(State#state.groupname),
      {reply,ok , ready, State#state{socket = Socket,log_flag=false}};
    {error,Reason} ->
      case State#state.log_flag of 
	false ->
	  %% Only log the first time an error occured
	  error_logger:warning_report([{subsystem,"ERPC"},
				       {description,"ERPC Connection - connection failure"},
				       {pid,self()},
				       {address,State#state.address},
				       {port,State#state.port},
				       {reason,Reason}]);
	true ->
	  ok
      end,

%      io:format("ERPC: Failed to connected to ~s:~p (~w)\n",[State#state.address,State#state.port,Reason]),
      erpc:remove_ready_connection(State#state.groupname),
      {reply,ok, error, State#state{log_flag=true}}
  end;
  
error({call,_M,_F,_A},_From,State) ->
  {reply,{error,connection_not_ready},error,State}.


%%--------------------------------------------------------------------
handle_sync_event(status, _From, StateName, State) ->
  {reply, {ok,StateName}, StateName, State}.

%%--------------------------------------------------------------------
handle_info({tcp_closed,_Socket}, _ReadyOrError, State) -> 
  erpc:remove_ready_connection(State#state.groupname),      
  error_logger:warning_report([{subsystem,"ERPC"},
			       {description,"ERPC Connection - Disconnected from remote host"},
			       {pid,self()},
			       {address,State#state.address},
			       {port,State#state.port}]),
  {next_state, error, State#state{socket=undefined}};
handle_info({tcp_error,_Socket,Reason}, ready, State) -> 
  error_logger:warning_report(["ERPC Connection - TCP Error",
			       {pid,self()},
			       {address,State#state.address},
			       {port,State#state.port},
			       {reason,Reason}]),
  erpc:remove_ready_connection(State#state.groupname),
  {next_state, error, State#state{socket=undefined}};  

handle_info({'EXIT',HeartPID,_Reason}, _State, State) when State#state.heartbeat == HeartPID -> 
  {stop,heart_exit,State};
  
handle_info({tcp,Socket,Data}, ready, State) -> 
  Packet = binary_to_term(Data),
  case Packet of
    {rpc_result,OrigFrom,Result} ->
      gen_fsm:reply(OrigFrom,Result);
    {stream_call,StreamRecvPID,Message,From} ->
      ok = do_stream_call(Socket,StreamRecvPID,Message,From);
    {stream_done,StreamRecvPID,From} ->
      ok = do_stream_close(Socket,StreamRecvPID,From)
  end,
  {next_state, ready, State};
handle_info(UnexpectedMessage,StateName,State) ->
  error_logger:warning_report([{subsystem,"ERPC"},
			       {description,"ERPC Connection - Unexpected message received"},
			       {pid,self()},
			       {unexpected_message,UnexpectedMessage},
			       {state_name,StateName},
			       {state,State}]),
  {next_state,StateName,State}.

%%--------------------------------------------------------------------
terminate(_Reason, _StateName, State) ->
  HeartPid = State#state.heartbeat,
  exit(HeartPid,killed),
  ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.


do_stream_call(Socket,StreamRecvPID,Message,From) ->
  proc_lib:spawn(fun() ->
		     ok = erpc_stream_recv:stream_call(StreamRecvPID,Message),
		     ok = send_ok_reply(Socket,From),
		     normal
		 end),
  ok.
		 
		     

do_stream_close(Socket,StreamRecvPID,From) ->
  proc_lib:spawn(fun() ->
		     {ok,_FinalState} = erpc_stream_recv:stream_close(StreamRecvPID),
		     ok = send_ok_reply(Socket,From),
		     normal
		 end),
  ok.

send_ok_reply(Socket,From) ->
  Reply = {reply,From,ok},
  Packet = term_to_binary(Reply),
  ok = gen_tcp:send(Socket,Packet).
  

log(_State,String,Params) ->
  ok.
%  io:format(String ++ "\n",Params).






start_heartbeat(Interval) ->
  Owner = self(),
  spawn_link(?MODULE,go,[Owner,Interval]).

go(Owner,Interval) ->
  run(Owner,Interval).

run(Owner,Interval) ->
  timer:sleep(Interval),
  catch gen_fsm:sync_send_event(Owner,heartbeat,600000),
  ?MODULE:run(Owner,Interval).
