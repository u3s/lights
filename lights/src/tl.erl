%%%-------------------------------------------------------------------
%%% @author  <matt@liliput>
%%% @copyright (C) 2015, 
%%% @doc
%%% Main server module. Includes functions for creating, handling and
%%% removing FSM processes representing traffic lights units.
%%% @end
%%% Created : 23 Aug 2015 by  <matt@liliput>
%%%-------------------------------------------------------------------
-module(tl).

-behaviour(gen_server).

-include("../include/tl.hrl").
%% API
-export([start_link/0, stop/0]).
-export([add/0, remove/1, remove_all/0, list/0, lookup/1, start_cycle/1,
	 set_emergency/1, get_emergency/1, stop_cycle/1, get_lamps/1]).

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
	  robots :: [] | [#robot{}],	% list of active tl_fsm robots
	  next_robot :: integer()
	 }).

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
%%--------------------------------------------------------------------
%% @doc
%% Adds new FSM process representing single traffic lights robot
%%
%% @spec add() -> ok
%% @end
%%--------------------------------------------------------------------
add() ->
    gen_server:cast(?SERVER, add).

%%--------------------------------------------------------------------
%% @doc
%% Kills the FSM process if not active (soft removal of single unit)
%%
%% @spec remove(Name) -> ok | {warning, robot_active} |
%%                            {error, does_not_exist}
%% @end
%%--------------------------------------------------------------------
remove(Name) ->
    gen_server:cast(?SERVER, {remove, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Kills all FSM processes unconditionally (forced removal).
%% If any FSM process is active, i.e. has spawned switching process,
%% this process is killed (message {'DOWN', ref(),process,pid(),stop}
%% is sent to the server), then the FSM process is terminated.
%%
%% @spec remove_all() -> ok
%% @end
%%--------------------------------------------------------------------
remove_all()  ->
    gen_server:cast(?SERVER, remove_all).

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of #robot{} records, defined earlier by means of
%% add/0. If no robots are present, empty list is returned.
%%
%% @spec list() -> [#robot{}] | []
%% @end
%%--------------------------------------------------------------------
list() ->
    gen_server:call(?SERVER, list).

%%--------------------------------------------------------------------
%% @doc
%% Returns a tuple {ok, Name} if the process with given Name exists,
%% otherwise {nok, Name} is returned.
%%
%% @spec lookup(Name) -> {ok, Name} | {nok, Name}
%% @end
%%--------------------------------------------------------------------
lookup(Name) ->
    gen_server:call(?SERVER, {lookup, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Start new process driving existing FSM, switching its states
%% in endless loop, until killed by stop_cycle/1, remove_all/0 or
%% exit BIF.
%%
%% @spec start_cycle(Name) -> ok
%% @end
%%--------------------------------------------------------------------
start_cycle(Name) ->
    gen_server:cast(?SERVER, {start_cycle, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Tries to kill the process spawned in start_cycle/1, if its Pid
%% is found on State.robots list and if is alive. Otherwise only
%% removes the Pid from the list.
%%
%% @spec stop_cycle(Name) -> ok
%% @end
%%--------------------------------------------------------------------
stop_cycle(Name) ->
    gen_server:cast(?SERVER, {stop_cycle, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Sets FSM process state to emergency. If the process does not exist,
%% info is printed.
%%
%% @spec set_emergency(Name) -> ok
%% @end
%%--------------------------------------------------------------------
set_emergency(Name) ->
    gen_server:cast(?SERVER, {set_emergency, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Gets FSM process emergency flag.
%%
%% @spec get_emergency(Name) -> emergency | normal | {error, does_not_exist}
%% @end
%%--------------------------------------------------------------------
get_emergency(Name) ->
    gen_server:call(?SERVER, {get_emergency, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Gets FSM process lamps status.
%%
%% @spec get_lamps(Name) -> #lamps{} | {error, does_not_exist}
%% @end
%%--------------------------------------------------------------------
get_lamps(Name) ->
    gen_server:call(?SERVER, {get_lamps, Name}).

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
    %% logger must be started first
    tl_logger:start_server(?SERVER),
    {ok, #state{
	    timer_red = ?TIME_R,
	    timer_red_yellow = ?TIME_RY,
	    timer_green = ?TIME_G,
	    timer_yellow = ?TIME_Y,
	    robots = [],
	    next_robot = 1
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
handle_call(list, _From, State) ->
    Reply = State#state.robots,
    {reply, Reply, State};

handle_call({lookup, Name}, _From, State) when is_list(Name) ->
    Reply =
	case lists:keyfind(Name, 2, State#state.robots) of
	    #robot{id=Id} ->
		{ok, Id};
	    false ->
		{{error, not_found}, Name}
	end,
    {reply, Reply, State};

handle_call({get_emergency, Name}, _From, State) ->
    Reply =
	case lists:keyfind(Name, 2, State#state.robots) of
	    #robot{pid=RPid} ->
		tl_fsm:get_emergency_state(RPid);
	    false ->
		{error, does_not_exist}
	end,
    {reply, Reply, State};

handle_call({get_lamps, Name}, _From, State) ->
    Reply =
	case lists:keyfind(Name, 2, State#state.robots) of
	    #robot{pid=RPid} ->
		tl_fsm:get_lamps(RPid);
	    false ->
		{error, does_not_exist}
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
handle_cast(add, State) ->
    ChildSpec =
	#{id => "robot/" ++ integer_to_list(State#state.next_robot),
	  start => {tl_fsm, start_link, []},
	  period => 1},
    {ok, RobotPid} = supervisor:start_child(lights_sup, ChildSpec),
    #{id := Name} = ChildSpec,
    tl_logger:add(Name),
    {noreply, State#state{
		robots=lists:append([#robot{id=Name, pid = RobotPid}],
				    State#state.robots),
		next_robot=State#state.next_robot+1}};

handle_cast({remove, Name}, State) ->
    case lists:keyfind(Name, 2, State#state.robots) of
	#robot{id=Name} ->
	    case is_switching(Name, State) of
		false ->
		    supervisor:terminate_child(lights_sup, Name),
		    supervisor:delete_child(lights_sup, Name),
		    tl_logger:remove(Name),
		    {noreply, State#state{robots=lists:keydelete(Name, 2,
								State#state.robots)}};
		true ->
		    tl_logger:remove_error(Name, refused_robot_active),
		    {noreply, State}
	    end;
	false ->
	    tl_logger:remove_error(Name, does_not_exist),
	    {noreply, State}
    end;

handle_cast({start_cycle, Id}, State) ->
    case lists:keyfind(Id, 2, State#state.robots) of
	#robot{id=Id, pid=RPid, spid=SPid}=OldRobot ->
	    case SPid of
		_Pid when is_pid(_Pid) ->
		    tl_logger:start_switch_error(Id, already_switching),
		    {noreply, State};
		undefined ->
		    case tl_fsm:get_emergency_state(RPid) of
			emergency ->
			    tl_fsm:set_normal(RPid),
			    tl_logger:normal(Id);
			normal ->
			    ok
		    end,
		    {NewSPid, _MRef} = spawn_monitor(?MODULE, switch_loop, [Id, RPid, State]),
		    tl_logger:start_switch(Id, NewSPid),
	    	    NewState = State#state{
				 robots=
				     lists:keyreplace(Id, 2, State#state.robots,
						      OldRobot#robot{spid=NewSPid})},
		    {noreply, NewState}
	    end;
	false ->
	    tl_logger:start_switch_error(Id, not_found),
	    {noreply, State}
    end;

handle_cast({stop_cycle, Id}, State) ->
    case lists:keyfind(Id, 2, State#state.robots) of
	#robot{id=Id, spid=SPid}=OldRobot ->
	    NewState =
		kill_switching_process(Id, SPid, OldRobot, State),
	    {noreply, NewState};
	false ->
	    tl_logger:stop_switch_error(Id, not_found),
	    {noreply, State}	 
    end;

handle_cast(remove_all, State) ->
    lists:foreach(fun(Id) ->
			case is_switching(Id, State) of
			    false ->
				supervisor:terminate_child(lights_sup, Id),
				supervisor:delete_child(lights_sup, Id),
				tl_logger:remove(Id);
			    true ->
				#robot{spid=SPid} =
				    lists:keyfind(Id, 2, State#state.robots),
				exit(SPid, normal),
				tl_logger:stop_switch(Id),
				supervisor:terminate_child(lights_sup, Id),
				supervisor:delete_child(lights_sup, Id),
				tl_logger:remove(Id)
			end
		  end,
		[R_Id || #robot{id=R_Id} <- State#state.robots]),
    {noreply, State#state{robots=[]}};

handle_cast({set_emergency, Id}, State) ->
    case lists:keyfind(Id, 2, State#state.robots) of
	#robot{pid=RPid, spid=SPid}=OldRobot ->
	    NewState =
		kill_switching_process(Id, SPid, OldRobot, State),
	    tl_fsm:set_emergency(RPid),
	    tl_logger:emergency(Id),
	    {noreply, NewState};
	false ->
	    tl_logger:emergency_error(Id, not_found),
	    {noreply, State}	 
    end;

handle_cast(stop, State) ->
    tl_logger:stop_server(?SERVER),
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
handle_info({'DOWN', _Ref, process, Pid, stop}, State) ->
    tl_logger:down(Pid),
    {noreply, State};

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
    LampsRY = tl_fsm:switch_next(Pid),
    tl_logger:switch(Id, LampsRY, red_yellow),
    timer:sleep(State#state.timer_red_yellow),

    LampsG = tl_fsm:switch_next(Pid),
    tl_logger:switch(Id, LampsG, green),
    timer:sleep(State#state.timer_green),

    LampsY = tl_fsm:switch_next(Pid),
    tl_logger:switch(Id, LampsY, yellow),
    timer:sleep(State#state.timer_yellow),

    LampsR = tl_fsm:switch_next(Pid),
    tl_logger:switch(Id, LampsR, red),
    timer:sleep(State#state.timer_red),

    switch_loop(Id, Pid, State).

is_switching(Id, State) ->
    #robot{spid=SPid} = lists:keyfind(Id, 2, State#state.robots),
    case SPid of
	undefined ->
	    false;
	Pid when is_pid(Pid) ->
	    case is_process_alive(Pid) of
		true ->
		    true;
		false ->
		    false
	    end
    end.

kill_switching_process(Id, SPid, OldRobot, State)
  when is_pid(SPid) ->
    case is_process_alive(SPid) of
	true ->
	    exit(SPid, stop),
	    false = is_process_alive(SPid), % debug code
	    tl_logger:stop_switch(Id);
	false ->
	    tl_logger:stop_switch_error(Id, already_killed)
    end,
    State#state{robots=
		    lists:keyreplace(Id, 2,
				     State#state.robots,
				     OldRobot#robot{spid=undefined})};
kill_switching_process(Id, _SPid, _OldRobot, State)
  when _SPid == undefined ->
    tl_logger:stop_switch_error(Id, not_switching),
    State.
