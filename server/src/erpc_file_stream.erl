%%%-------------------------------------------------------------------
%%% File    : erpc_file_stream.erl
%%% Author  : Rudolph van Graan <>
%%% Description : 
%%%
%%% Created : 30 May 2007 by Rudolph van Graan <>
%%% Copyright: (C) 2006,2007 by Rudolph van Graan
%%%-------------------------------------------------------------------
-module(erpc_file_stream).

-behaviour(gen_server).

%% API
-export([start/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {stream_send_pid,
	        filename,
		options,
	        fd}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(StreamSendPID,Node,Filename,Options) ->
  ensure_cookie(Node),
  proc_lib:spawn(Node,gen_server,start,[?MODULE,[StreamSendPID,Filename,Options],[]]).

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
init([StreamSendPID,Filename,Options]) ->
  link(StreamSendPID),
  process_flag(trap_exit,true),
  {ok,FD} = file:open(Filename,[read,raw,binary]),
  timer:send_after(10,do),
  ok = log("Starting download for file ~s",[Filename]),

  {ok, #state{stream_send_pid=StreamSendPID,
	      filename=Filename,
	      options=Options,
	      fd=FD}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

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
handle_info(do, State) ->
  send_file(State).

send_file(State) ->
  case file:read(State#state.fd,1024) of
    {ok,Data} -> 
      case erpc_stream_send:stream_call(State#state.stream_send_pid,{chunk,Data}) of
	ok ->
	  send_file(State);
	{error,sink_error} ->
	  ok = log("File ~s not downloaded - sink error",[State#state.filename]),
	  {stop,normal,State}
      end;
    eof -> 
      ok = erpc_stream_send:stream_done(State#state.stream_send_pid),
      ok = log("File ~s download complete",[State#state.filename]),
      {stop,normal,State};
    {error,Reason} -> 
      {stop,{error,Reason},State}
  end.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
  ok = do_delete_file(State),
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

do_delete_file(State) ->
  case lists:member(delete_file,State#state.options) of
    true ->
      ok = log("Deleted download file ~s",[State#state.filename]),
      ok = file:delete(State#state.filename);
    false ->
      ok
  end.

log(String,Params) ->
  ok.
%  system_logger:erpc_log("ERPC: "++String,Params).

ensure_cookie(Node) ->
  case application:get_env(erpc_srv,stream_cookies) of
    undefined -> 
      ok;
    {ok,StreamCookies} -> 
	case params:fget(Node,StreamCookies,undefined) of
	  undefined ->
	    ok;
	  Cookie ->
	    erlang:set_cookie(Node,Cookie)
	end
    end.
  
  
  
