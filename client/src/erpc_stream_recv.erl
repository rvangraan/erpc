%%%-------------------------------------------------------------------
%%% File    : erpc_stream_recv.erl
%%% Author  : Rudolph van Graan <>
%%% Description : 
%%% Copyright: (C) 2006 by Rudolph van Graan
%%% Created : 28 May 2007 by Rudolph van Graan <>
%%%-------------------------------------------------------------------
-module(erpc_stream_recv).

-behaviour(gen_server).

%% API
-export([start_link/3,
	 stream_call/2,
	 stream_close/1,
	 stream_send_pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {groupname,
		module_name,
		module_state,
	        stream_send_pid,
		connection,
	        owner_monitor,
	        connection_monitor}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Groupname,ModuleName,InitParams) ->
  Owner = self(),
  gen_server:start_link(?MODULE, [Owner,Groupname,ModuleName,InitParams], []).

stream_call(PID,Message) ->
  gen_server:call(PID,{stream_call,Message},600000).

stream_close(PID) ->
  gen_server:call(PID,stream_close).

stream_send_pid(PID) ->
  gen_server:call(PID,stream_send_pid).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Owner,Groupname,ModuleName,InitParams]) ->
  StreamPid = self(),
  process_flag(trap_exit,true),
  {ok,ModuleState} = ModuleName:erpc_stream_init(StreamPid,InitParams),
  {ok,StreamSendPID,Connection} = erpc:create_remote_stream_sender(Groupname,StreamPid),
  OwnerMonitor = erlang:monitor(process,Owner),
  ConnMonitor  = erlang:monitor(process,Connection),
  {ok, #state{groupname          = Groupname,
	      module_name        = ModuleName,
	      module_state       = ModuleState,
	      connection         = Connection,
	      owner_monitor      = OwnerMonitor,
	      connection_monitor = ConnMonitor,
	      stream_send_pid    = StreamSendPID}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stream_send_pid, _From, State) ->
  {reply,State#state.stream_send_pid,State};
handle_call({stream_call,Message}, _From, State) ->
  ModuleName = State#state.module_name,
  {ok,NewState} = ModuleName:erpc_stream_message(self(),Message,State#state.module_state),
  Reply = ok,
  {reply,Reply , State#state{module_state=NewState}};
handle_call(stream_close, _From, State) ->
  ModuleName = State#state.module_name,
  {ok,NewState} = ModuleName:erpc_stream_closed(self(),State#state.module_state),
  Reply = {ok,NewState},
  {stop,normal,Reply, State#state{module_state=NewState}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN',Monitor,process,_OwnerPID,_Reason}, State) when State#state.owner_monitor == Monitor->
  io:format("Owner died for stream recv!\n"),
  %% Drop the send pid - no use deallocating it when the connection died
  {stop,normal,State#state{stream_send_pid=undefined}};
handle_info({'DOWN',Monitor,process,_ConnectionPID,_Reason}, State) when State#state.connection_monitor == Monitor->
  io:format("Connection died for stream recv!\n"),
  %% Drop the send pid - no use deallocating it when the connection died
  {stop,normal,State#state{stream_send_pid=undefined}}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
  ok = erpc:deallocate_remote_stream_sender(State#state.connection,
					    State#state.stream_send_pid),
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
