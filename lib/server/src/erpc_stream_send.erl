%%%-------------------------------------------------------------------
%%% File    : erpc_stream_send.erl
%%% Author  : Rudolph van Graan <>
%%% Description : 
%%%
%%% Created : 28 May 2007 by Rudolph van Graan <>
%%% Copyright: (C) 2006,2007 by Rudolph van Graan
%%%-------------------------------------------------------------------
-module(erpc_stream_send).

-behaviour(gen_server).

%% API
-export([start_link/2,
	       deallocate/1,
	       stream_call/2,
	       stream_done/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,{ monitor,
		            connection_endpoint,
		            stream_recv_pid }).

%%--------------------------------------------------------------------

start_link(ConnectionEndPoint,StreamRecvPID) ->
  Owner = self(),
  gen_server:start_link(?MODULE, [Owner,ConnectionEndPoint,StreamRecvPID], []).

deallocate(RemoteStreamSendPID) ->
  catch gen_server:cast(RemoteStreamSendPID,stop),
  ok.

stream_call(StreamSendPID,Message) ->
  try
    gen_server:call(StreamSendPID,{stream_call,Message},120000)
  catch
    exit:_E ->
      {error,sink_error}
  end.

stream_done(StreamSendPID) ->
  gen_server:call(StreamSendPID,stream_done).

%%--------------------------------------------------------------------

init([Owner,ConnectionEndPoint,StreamRecvPID]) ->
  process_flag(trap_exit,true),
  Monitor = erlang:monitor(process,Owner),
  {ok, #state{monitor       = Monitor,
	      connection_endpoint = ConnectionEndPoint,
	      stream_recv_pid     = StreamRecvPID}}.

handle_call({stream_call,Message}, _From, State) ->
  Reply = erpc_connection_endpoint:stream_call(State#state.connection_endpoint,State#state.stream_recv_pid,Message),
  {reply, Reply, State};
handle_call(stream_done,_From,State) ->
  ok = erpc_connection_endpoint:stream_done(State#state.connection_endpoint,State#state.stream_recv_pid),
  {reply,ok,State}.

handle_cast(stop, State) ->
  {stop,normal, State}.

handle_info({'DOWN',_Monitor,process,_OwnerPID,_Reason}, State) ->
  {stop,normal, State};
handle_info({'EXIT',_PID,normal},State) ->
  {noreply,State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.