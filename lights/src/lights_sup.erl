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
    ChildSpec =
	#{id => tl,
	  start => {tl, start_link, []},
	  restart => permanent,
	  shutdown => 60000,
	  type => worker, 
	  modules => [tl]},
    {ok, { SupFlags, [ChildSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
