%%%-------------------------------------------------------------------
%% @doc lights top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('lights_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags =
	#{strategy => one_for_all,
	  intensity => 0,
	  period => 1},
    Tl_server =
	#{id => tl,
	  start => {tl, start_link, []},
	  restart => permanent,
	  shutdown => 60000,
	  type => worker, 
	  modules => [tl]},
    Tl_event_manager =
	#{id => tl_logger,
	  start => {tl_logger, start_link, []},
	  restart => permanent,
	  shutdown => 60000,
	  type => worker,
	  modules => [tl_logger]},
    Children = [Tl_event_manager, Tl_server],
    {ok, { SupFlags, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
