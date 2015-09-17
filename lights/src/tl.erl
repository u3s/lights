%%%-------------------------------------------------------------------
%%% @author  <matt@liliput>
%%% @copyright (C) 2015, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Aug 2015 by  <matt@liliput>
%%%-------------------------------------------------------------------
-module(tl).

-behaviour(gen_server).

-include("../include/tl.hrl").
%% API
-export([start_link/0, stop/0]).
-export([add/0, remove/1, list/0, lookup/1, start_cycle/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% other exports
-export([switch_loop/3]).

-define(SERVER, ?MODULE).
-define(TIME_R, 10000).
-define(TIME_RY, 2000).
-define(TIME_G, 10000).
-define(TIME_Y, 2000).

-record(state, {
	  timer_red = 0,
	  timer_red_yellow = 0,
	  timer_green = 0,
	  timer_yellow = 0,
	  robots = [#robot{}]	% list of active tl_fsm robots
	 }).

-record(robot, {
	  id,
	  pid  :: pid(),        % fsm process pid
	  spid :: pid()         % start_cycle process Pid
		  }

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

add() ->
    gen_server:call(?SERVER, add). % add timeout?

remove(Name) ->
    gen_server:call(?SERVER, {remove, Name}).

list() ->
    gen_server:call(?SERVER, list).

lookup(Name) ->
    gen_server:call(?SERVER, {lookup, Name}).

start_cycle(Name) ->
    gen_server:cast(?SERVER, {start_cycle, Name}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{
	    timer_red = ?TIME_R,
	    timer_red_yellow = ?TIME_RY,
	    timer_green = ?TIME_G,
	    timer_yellow = ?TIME_Y,
	   }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(add, _From, State) ->
    ChildSpec =
	#{id => "robot/" ++ integer_to_list(length(State#state.robots)+1),
	  start => {tl_fsm, start_link, []},
	  period => 1},
    {ok, RobotPid} = supervisor:start_child(lights_sup, ChildSpec),
    Reply = ok,
      {reply, Reply, State#state{robots=lists:append([
						      #robot{id=RobotPid], State#state.robots)}};
handle_call({remove, Name}, _From, State) ->
    case lists:keymember(Name, 1, supervisor:which_children(lights_sup)) of
	true -> supervisor:terminate_child(lights_sup, Name),
		supervisor:delete_child(lights_sup, Name),
		{reply, ok, State#state{robots=lists:delete(Name,
							    State#state.robots)}};
	false -> {reply, {error, not_existing_child, State}}
    end;
handle_call(list, _From, State) ->
    Reply = supervisor:which_children(lights_sup),
    {reply, Reply, State};
handle_call({lookup, Name}, _From, State) when is_list(Name) ->
    Reply =
	case lists:keyfind(Name, 1, supervisor:which_children(lights_sup)) of
	    {Id, _, _, _} ->
		{ok, Id};
	    false ->
		{nok, Name}
	end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({start_cycle, Id}, State) ->
    case lists:keyfind(Id, 1, supervisor:which_children(lights_sup)) of
	{Id, RPid,_,_} ->
	    SPid = spawn_link(?MODULE, switch_loop, [Id, RPid, State]);
	false -> io:format("Id ~p  not found, ~p", [Id, State])
    end,			       
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
switch_loop(Id, Pid, State) ->
    LampsR = gen_fsm:sync_send_event(Pid, switch_next),
    io:format("~p: R:~p Y:~p G:~p~n", [Id, LampsR#lamps.red,
				       LampsR#lamps.yellow,
				       LampsR#lamps.green]),
    timer:sleep(State#state.timer_red),
    LampsRY = gen_fsm:sync_send_event(Pid, switch_next),
    io:format("~p: R:~p Y:~p G:~p~n", [Id, LampsRY#lamps.red,
				       LampsRY#lamps.yellow,
				       LampsRY#lamps.green]),
    timer:sleep(State#state.timer_red_yellow),
    LampsG = gen_fsm:sync_send_event(Pid, switch_next),
    io:format("~p: R:~p Y:~p G:~p~n", [Id, LampsG#lamps.red,
				       LampsG#lamps.yellow,
				       LampsG#lamps.green]),
    timer:sleep(State#state.timer_green),
    LampsY = gen_fsm:sync_send_event(Pid, switch_next),
    io:format("~p: R:~p Y:~p G:~p~n", [Id, LampsY#lamps.red,
				       LampsY#lamps.yellow,
				       LampsY#lamps.green]),
    timer:sleep(State#state.timer_yellow),
    switch_loop(Id, Pid, State).
