-module(tl_logger).
-export([start_server/1, stop_server/1, add/1, remove/1, remove_error/2,
	 emergency/1, emergency_error/2, normal/1, switch/3, start_switch/2,
	 stop_switch/1, start_switch_error/2, stop_switch_error/2]).
-export([init_fsm/1, set_lamps/1, terminate_fsm/1]).
-export([start_link/0, add_handler/2, delete_handler/2, down/1]).
%% this is a wrapper module for event manager, which will route the events
%% to event handlers

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

start_server(Server) ->
    gen_event:notify(?SERVER, {start_server, Server}).

stop_server(Server) ->
    gen_event:notify(?SERVER, {stop_server, Server}).

add(Robot) ->
    gen_event:notify(?SERVER, {add, Robot}).

remove(Robot) ->
    gen_event:notify(?SERVER, {remove, Robot}).

remove_error(Robot, Reason) ->
    gen_event:notify(?SERVER, {remove_error, Robot, Reason}).

emergency(Robot) ->
    gen_event:notify(?SERVER, {emergency, Robot}).

emergency_error(Robot, Reason) ->
    gen_event:notify(?SERVER, {emergency_error, Robot, Reason}).

normal(Robot) ->
    gen_event:notify(?SERVER, {normal, Robot}).

switch(Robot, Lamps, RState) ->
    gen_event:notify(?SERVER, {switch, Robot, Lamps, RState}).

start_switch(Robot, Pid) ->
    gen_event:notify(?SERVER, {start_switch, Robot, Pid}).

start_switch_error(Robot, Reason) ->
    gen_event:notify(?SERVER, {start_switch_error,
			       Robot, Reason}).

stop_switch(Robot) ->
    gen_event:notify(?SERVER, {stop_switch, Robot}).

stop_switch_error(Robot, Reason) ->
    gen_event:notify(?SERVER, {stop_switch_error, Robot, Reason}).

down(Pid) ->
    gen_event:notify(?SERVER, {down, Pid}).

%% fsm related events
init_fsm(Name) ->
    gen_event:notify(?SERVER, {init_fsm, Name}).

set_lamps({Name, R, Y, G}) ->
    gen_event:notify(?SERVER, {set_lamps, {Name, R, Y, G}}).

terminate_fsm(Name) ->
    gen_event:notify(?SERVER, {terminate_fsm, Name}).
