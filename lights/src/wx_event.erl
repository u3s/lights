%%%-------------------------------------------------------------------
%%% @author  <matt@liliput>
%%% @copyright (C) 2015, 
%%% @doc
%%%
%%% @end
%%% Created : 20 Oct 2015 by  <matt@liliput>
%%%-------------------------------------------------------------------
-module(wx_event).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).
%% other exports
%-export([start_blinking/1]).


-define(SERVER, ?MODULE).
-record(wx_obj, {name,
		 wxpid :: undefined | pid()
		}).
-record(state, {wx_objects = [] :: [] | [#wx_obj{}]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    tl_logger:add_handler(?MODULE, []).

delete_handler() ->
    tl_logger:delete_handler(?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{wx_objects=[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({init_fsm, Name}, State) ->
    error_logger:info_msg("~s start reported to wx_event", [Name]),
    RwxPid = spawn_link(robot, start, [Name]),
    {ok, State#state{
	   wx_objects=
	       lists:append([#wx_obj{name=Name,wxpid=RwxPid}],
			    State#state.wx_objects)}};

handle_event({set_lamps, {Name, R, Y, G}}, State) ->
    #wx_obj{name=Name, wxpid=WxPid} =
	lists:keyfind(Name, 2, State#state.wx_objects),
    WxPid ! {R, Y, G},
    State,
    {ok, State};

handle_event({terminate_fsm, Name}, State) ->
    error_logger:info_msg("~s stop reported to wx_event", [Name]),
    #wx_obj{name=Name, wxpid=WxPid} =
	lists:keyfind(Name, 2, State#state.wx_objects),    
    exit(WxPid, stop),
    {ok, State#state{wx_objects=
			 lists:keydelete(Name, 2,
					State#state.wx_objects)}};

handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
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
%% start_blinking(Pid) ->
%%     Pid ! {off, off, off},
%%     timer:sleep(500),
%%     Pid ! {off, on, off}, 
%%     timer:sleep(500),
%%     start_blinking(Pid).
