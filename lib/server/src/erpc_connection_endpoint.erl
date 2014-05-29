%%%-------------------------------------------------------------------
%%% File    : erpc_connection_endpoint.erl
%%% Author  : Rudolph van Graan <>
%%% Description : This is the endpoint of an erpc tunnel
%%%
%%% Created : 11 Nov 2006 by Rudolph van Graan <>
%%% Copyright: (C) 2006,2007 by Rudolph van Graan

%%%-------------------------------------------------------------------
-module(erpc_connection_endpoint).

%% TCP Protocol:
%% Proxy    sends {rpc,OrigFrom,M,F,A}
%% Skeleton reply {rpc_reply,OrigFrom,Result}

-behaviour(gen_server).

%% API
-export([start_link/1,
	 complete/3,
	 stream_call/3,
	 stream_done/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket}).

%%--------------------------------------------------------------------

start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

complete(Connection,OrigFrom,Result) ->
  gen_server:call(Connection,{complete,OrigFrom,Result},infinity).

stream_call(Connection,StreamRecvPID,Message) ->
  try 
    gen_server:call(Connection,{stream_call,StreamRecvPID,Message},120000)
  catch
    exit:_E ->
      {error,sink_error}
  end.

stream_done(Connection,StreamRecvPID) ->
  gen_server:call(Connection,{stream_done,StreamRecvPID}).

%%--------------------------------------------------------------------

init([Socket]) ->
%  io:format("erpc connection endpoint started\n"),
  ok = inet:setopts(Socket,[{active,true},{packet,4},binary]),  
  {ok, #state{socket = Socket}}.

handle_call({complete,OrigFrom,Result}, _From, State) ->
  reply_to_original_caller(State,OrigFrom,Result),
  {reply, ok, State};
handle_call({stream_call,StreamRecvPID,Message},From,State) ->
  Term = {stream_call,StreamRecvPID,Message,From},
  Packet = term_to_binary(Term),
  ok = gen_tcp:send(State#state.socket,Packet),
  {noreply,State};
handle_call({stream_done,StreamRecvPID},From,State) ->
  Term = {stream_done,StreamRecvPID,From},
  Packet = term_to_binary(Term),
  ok = gen_tcp:send(State#state.socket,Packet),
  {noreply,State}.

handle_cast(_,State) ->
  {noreply,State}.

reply_to_original_caller(State,OrigFrom,Result) ->
  Term = {rpc_result,OrigFrom,Result},
  Packet = term_to_binary(Term),
  ok = gen_tcp:send(State#state.socket,Packet).

handle_info({tcp_closed,_Socket}, State) -> 
%  log(State,"connection closed",[]),
  {stop,normal,State};

handle_info({tcp_error,_Socket,_Reason}, State) ->
%  log(State,"connection closed, error: ~p",[Reason]),
  {stop,normal,State};

handle_info({tcp,_Socket,Data}, State) ->
  Command = binary_to_term(Data),
  ok = process_command(State,Command),
  {noreply,State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------

process_command(State,{echo,Seq}) ->
  Term = {echo_response,Seq},
  Packet = term_to_binary(Term),
  ok = gen_tcp:send(State#state.socket,Packet);
process_command(_State,{rpc,OrigFrom,M,F,A}) ->
  try
    proc_lib:spawn(erpc_call_skeleton,go,[self(),OrigFrom,M,F,A])
  catch error:{system_limit,_Stack} ->
      gen_server:reply(OrigFrom,{throw,resource_limit}) 
  end,
  ok;
process_command(State,{create_stream_sender,OrigFrom,StreamRecvPID}) ->
  {ok,StreamSendPID} = erpc_stream_send:start_link(self(),StreamRecvPID),
  reply_to_original_caller(State,OrigFrom,{ok,StreamSendPID}),
  ok;
process_command(_State,{reply,From,Result}) ->
  ok = do_reply(From,Result),
  ok;
process_command(_State,{deallocate_remote_stream_sender,RemoteStreamSendPID}) ->
  ok = erpc_stream_send:deallocate(RemoteStreamSendPID).

do_reply(From,Result) ->
  proc_lib:spawn(fun() ->
		     gen_server:reply(From,Result)
		 end),
  ok.

%log(_State,String,Params) ->
%  io:format(String++"\n",Params).
