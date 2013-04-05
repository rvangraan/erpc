%%%-------------------------------------------------------------------
%%% File    : erpc.erl
%%% Author  : Rudolph van Graan <>
%%% Description : 
%%% This module is the controller for the erpc client side
%%% All erpc calls go through the GS
%%% Created : 13 Nov 2006 by Rudolph van Graan <>
%%% Copyright: (C) 2006 by Rudolph van Graan
%%%-------------------------------------------------------------------
-module(erpc).

-behaviour(gen_server).

%% API
-export([start_link/2,
	 connection_count/1,
	 get_ready_connection/1,
	 add_ready_connection/1,
	 remove_ready_connection/1,
	 create_stream/3,
	 close_stream/1,
	 create_remote_stream_sender/2,
	 deallocate_remote_stream_sender/2,
	 call/3,
	 call/4,
	 status/1,
	 extended_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {groupname,
		owner,
		monitor,
		connections=[],
	        ready_connections=[]}).

%%--------------------------------------------------------------------
%%Backends -> [{Address,Port}] 
start_link(Groupname,Backends) ->
  Owner = self(),
  gen_server:start_link(?MODULE, [Owner,Groupname,Backends], []).

where(G) ->
  R = gproc:where({n,l,{erpc,G}}),
  case R of
    undefined ->
      throw({erpc,{no_backend,G}});
    R ->
      R
  end.
  
      

connection_count(G) ->
  gen_server:call(where(G),connection_count).

status(G) ->
  gen_server:call(where(G),status).


extended_status(G) ->
  gen_server:call(where(G),extended_status).


add_ready_connection(G) ->
  catch 
    gen_server:call(where(G),{add_ready_connection,self()}).

get_ready_connection(G) ->
  gen_server:call(where(G),get_ready_connection).

call(M,F,A) ->
  call(default_group,M,F,A).

call(G,M,F,A) ->
  case in_mnesia_transaction() of
    true  -> throw(erpc_mnesia_transaction_active);
    false -> ok
  end,

  case get_ready_connection(G) of
    {ok,Connection} ->
      erpc_connection:call(Connection,M,F,A);
    {error,no_backend} ->
      throw({erpc,{no_backend,G}})
  end.
	
remove_ready_connection(G) ->
  catch
    gen_server:call(where(G),{remove_ready_connection,self()}).


create_stream(Groupname,ModuleName,InitParams) ->
  {ok,_StreamPid} = erpc_stream_recv:start_link(Groupname,ModuleName,InitParams).

close_stream(StreamPID) ->
  erpc_stream_recv:stream_close(StreamPID).

create_remote_stream_sender(G,StreamRecvPID) ->
  case get_ready_connection(G) of
    {ok,Connection} ->
      {ok,RemoteStreamSendPID} = erpc_connection:create_remote_stream_sender(Connection,StreamRecvPID),
      {ok,RemoteStreamSendPID,Connection};
    {error,no_backend} ->
      throw({erpc,{no_backend,G}})
  end.

deallocate_remote_stream_sender(Connection,RemoteStreamSendPID) ->
  ok = erpc_connection:deallocate_remote_stream_sender(Connection,RemoteStreamSendPID).
  
  
  
  

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
init([Owner,Groupname,Backends]) ->
  process_flag(trap_exit,true),
  true = gproc:reg({n,l,{erpc,Groupname}},self()),
  Monitor = erlang:monitor(process,Owner),
  Connections = start_connections(Groupname,Backends),
  {ok, #state{owner=Owner,
	      groupname=Groupname,
	      monitor=Monitor,
	      connections=Connections}}.

%%--------------------------------------------------------------------

handle_call(get_ready_connection,_From, State) when State#state.ready_connections == [] ->
  Reply = {error,no_backend},
  {reply, Reply, State};
handle_call(get_ready_connection,_From, State) when State#state.ready_connections =/= [] ->
  [NextConnection|Rest] = State#state.ready_connections,
  Reply = {ok, NextConnection},
  {reply, Reply, State#state{ready_connections = Rest ++ [NextConnection]}};
  
handle_call({add_ready_connection,PID}, _From, State) ->
  ReadyConnections = (State#state.ready_connections -- [PID]) ++ [PID],
  Reply = ok,
  {reply, Reply, State#state{ready_connections = ReadyConnections}};

handle_call({remove_ready_connection,PID}, _From, State) ->
  ReadyConnections = State#state.ready_connections -- [PID],
  Reply = ok,
  {reply, Reply, State#state{ready_connections = ReadyConnections}};

handle_call(connection_count, _From, State) ->
  Reply = {ok,length(State#state.connections)},
  {reply, Reply, State};

handle_call(status, _From, State) when State#state.ready_connections == [] ->
  Reply = {ok,no_backend},
  {reply, Reply, State};

handle_call(status, _From, State) when State#state.ready_connections =/= [] ->
  Reply = {ok,ready},
  {reply, Reply, State};

handle_call(extended_status, _From, State) when State#state.ready_connections =/= [] ->
  Reply = {ok,ready,State#state.ready_connections},
  {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------

handle_info({'EXIT',PID,Reason},State) ->
  {stop,{connection_exit,PID,Reason},State};
handle_info({'DOWN',Monitor,process,_Object,_Other}, State) when Monitor == State#state.monitor->
  {stop,normal,State}.

%%--------------------------------------------------------------------
terminate(_Reason, State) ->
  Connections = State#state.connections,
  lists:foreach(fun(PID) -> ok = shutdown_connection(PID) end,Connections),
  ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------

start_connections(Groupname,Backends) ->
  F = fun({Address,Port}) ->
	  {ok,PID} = erpc_connection:start_link(Groupname,Address,Port),
	  PID
      end,      
  lists:map(F,Backends).

shutdown_connection(PID) ->
  Monitor = erlang:monitor(process,PID),
  exit(PID,shutdown),
  receive
    {'DOWN',Monitor,process,_Object,_Other} ->
      ok
  end.


%%--------------------------------------------------------------------
%% @doc Returns whether or not the current process has an active Mnesia transaction or not
%% @end
-spec in_mnesia_transaction() -> true | false.
in_mnesia_transaction() ->
    case get(mnesia_activity_state) of
        {_, _ActivityId, _Opaque} ->  
	    true;
        _ ->
	    false
    end.
