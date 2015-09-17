%%%-------------------------------------------------------------------
%%% @author  <matt@liliput>
%%% @copyright (C) 2015, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2015 by  <matt@liliput>
%%%-------------------------------------------------------------------
-module(tl_fsm).

-behaviour(gen_fsm).

-include("../include/tl.hrl").

%% API
-export([start_link/0]).
-export([switch_next/1, set_emergency/1, set_normal/1]).
%% gen_fsm callbacks
-export([init/1, red/3, red_yellow/3, green/3, yellow/3,
	 emergency/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%-define(SERVER, ?MODULE).

-record(state, {
	  pid,
	  lamps = #lamps{}
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

switch_next(Pid) ->
    gen_fsm:sync_send_event(Pid, switch_next).

set_emergency(Pid) ->
    gen_fsm:send_all_state_event(Pid, set_emergency).

set_normal(Pid) ->
    gen_fsm:send_event(Pid, normal).
 
%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    LampsInit = set_lamps(on, off, off),
    {ok, yellow, #state{
		    pid = self(),
		    lamps = LampsInit
		   }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
emergency(normal, State) ->
    {next_state, red, State#state{lamps = set_lamps(on, off, off)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
red(switch_next, _From, State) ->
    NewState = State#state{lamps = set_lamps(on, on, off)},
    Reply = NewState#state.lamps,
    {reply, Reply, red_yellow, NewState}.
red_yellow(switch_next, _From, State) ->
    NewState = State#state{lamps = set_lamps(off, off, on)},
    Reply = NewState#state.lamps,
    {reply, Reply, green, NewState}.
green(switch_next, _From, State) ->
    NewState = State#state{lamps = set_lamps(off, on, off)},
    Reply = NewState#state.lamps,
    {reply, Reply, yellow, NewState}.
yellow(switch_next, _From, State) ->
    NewState = State#state{lamps = set_lamps(on, off, off)},
    Reply = NewState#state.lamps,
    {reply, Reply, red, NewState}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(set_emergency, _StateName, State) ->
    {next_state, emergency, State#state{lamps = set_lamps(off, blinking, off)}};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
set_lamps(Red, Yellow, Green) ->
    NewLamp = #lamps{red = Red,
		     yellow = Yellow,
		     green = Green
		    },
    NewLamp.
