-module(tl_tests).
-include("../include/tl.hrl").
-include_lib("eunit/include/eunit.hrl").

tl_test_() ->
      {setup,
       fun setup/0,
       fun cleanup/1,
       {inorder,
	[fun list_empty/0,
	 fun add/0,
	 fun lookup_ok/0,
	 fun lookup_nok/0,
	 fun list/0,
	 fun start_cycle/0,
	 fun start_cycle_already_cycling/0,
	 fun try_remove_cycling/0,
	 fun stop_cycle/0,
	 fun stop_cycle_not_cycling/0,
	 fun remove/0,
	 fun remove_not_existing/0,
	 fun start_cycle_not_existing/0,
	 fun stop_cycle_not_existing_robot/0,
	 fun add_10/0,
	 fun remove_all/0,
	 fun add_10_start_cycle_5/0,
	 fun remove_all_some_switching/0,
	 fun kill_switch_process_then_stop_cycle/0,
	 fun set_emergency/0,
	 fun get_emergency/0,
	 fun start_cycle_then_set_emergency/0,
	 fun get_emergency_not_existing/0,
	 fun get_lamps/0,
	 fun get_lamps_not_existing/0
	]
       }}.

setup() ->
    application:start(lights),
    tl_event:add_handler(),
    ok.

cleanup(_) ->
    application:stop(lights).

add() ->
    ?assertEqual([], tl:list()),
    tl:add(),
    [{robot, R1, _, undefined}] = tl:list(),
    ?assertMatch([{robot, R1, _P1, undefined}]
		 when is_list(R1) andalso
		      is_pid(_P1),
		      tl:list()),
    {"robot/", _Id1}=lists:split(6,R1).

remove() ->
    ?assertMatch([{robot, R1, _P1, undefined}]
		 when is_list(R1) andalso
		      is_pid(_P1),
		      tl:list()),
    [{robot, R1, _, undefined}] = tl:list(),
    tl:remove(R1),
    ?assertEqual([], tl:list()).

remove_not_existing() ->
    ?assertMatch([], tl:list()),
    ok = tl:remove("robot/100"),
    ?assertMatch([], tl:list()).

lookup_ok() ->
    [{robot, R1, _, undefined}] = tl:list(),
    ?assertEqual({ok, R1}, tl:lookup(R1)).

lookup_nok() ->
    ?assertEqual({{error, not_found}, "robot/200"}, tl:lookup("robot/200")).

list() ->
    tl:add(),
    [{robot, R2, _, undefined},
     {robot, R1, _, undefined}] = tl:list(),
    ?assertMatch([{robot, R2, _RPid2, undefined},
		  {robot, R1, _RPid1, undefined}]
		 when is_list(R2) andalso
		      is_list(R1) andalso
		      is_pid(_RPid2) andalso
		      is_pid(_RPid1),
		      tl:list()),
    {"robot/", LId2} = lists:split(6, R2),
    {"robot/", LId1} = lists:split(6, R1),
    Id1 = list_to_integer(LId1),
    Id2 = list_to_integer(LId2),
    ?assertEqual(Id2, Id1+1),
    tl:remove(R2).

list_empty() ->
    ?assertEqual([], tl:list()).

start_cycle() ->
    [{robot, R1, _RPid, undefined} | _Rest] = tl:list(),
    ok = tl:start_cycle(R1),
    ?assertMatch([{robot, R1, _, SPid}]
		 when is_pid(SPid),
		      tl:list()).

stop_cycle() ->
    [{robot, R1, _RPid, _Spid} | _Rest] = tl:list(),
    ?assertMatch([{robot, R1, _, _SPid} | _Rest]
		 when is_pid(_SPid),
		      tl:list()),
    ok = tl:stop_cycle(R1),
    ?assertMatch([{robot, R1, _, undefined} | _Rest],
		 tl:list()).

try_remove_cycling() ->
    [{robot, R1, _, _} | _Rest] = tl:list(),
    ?assertMatch([{robot, R1, _, _SPid}]
		 when  is_pid(_SPid),
		       tl:list()),
    ok = tl:remove(R1),
    ?assertMatch([{robot, R1, _, _SPid}]
		 when is_pid(_SPid),
		      tl:list()).

stop_cycle_not_cycling() ->
    [{robot, R1, _RPid, undefined} | _Rest] = tl:list(),
    ?assertMatch([{robot, R1, _, undefined}],
		 tl:list()),
    ok = tl:stop_cycle(R1),
    ?assertMatch([{robot, R1, _, undefined}],
		 tl:list()).

start_cycle_already_cycling() ->
    [{robot, R1, _RPid, _} | _Rest] = tl:list(),
    ?assertMatch([{robot, R1, _, _SPid}]
		 when is_pid(_SPid),
		      tl:list()),
    ok = tl:start_cycle(R1),
    ?assertMatch([{robot, R1, _, _SPid}]
		 when is_pid(_SPid),
		      tl:list()).    

start_cycle_not_existing() ->
    ?assertEqual([], tl:list()),
    ok = tl:start_cycle("robot/303"),
    ?assertEqual([], tl:list()).

stop_cycle_not_existing_robot() ->
    ?assertEqual([], tl:list()),
    ok = tl:stop_cycle("robot/409"),
    ?assertEqual([], tl:list()).

add_10() ->
    Run =  fun Loop(0) ->
		   ok;
	       Loop(N) ->
		   tl:add(),
		   Loop(N-1)
	   end,
    Run(10),
    ?assertEqual(10,length(tl:list())).

remove_all() ->
    ?assertEqual(10,length(tl:list())),    
    ?assertEqual(12,				% event manager
		 length(supervisor:
			    which_children(lights_sup))),
    ok = tl:remove_all(),
    ?assertEqual([], tl:list()),
    ?assertEqual(2,				% event manager
		 length(supervisor:
			    which_children(lights_sup))).    

add_10_start_cycle_5() ->
    Run =  fun Loop(0) ->
		   ok;
	       Loop(N) when N rem 2 =:= 0 ->
		   tl:add(),
		   Loop(N-1);
	       Loop(N) ->
		   Before = tl:list(),
		   tl:add(),
		   After = tl:list(),
		   [#robot{id=JustAdded} | Before] = After, % test and decompose
		   tl:start_cycle(JustAdded),
		   Loop(N-1)
	   end,
    Run(10),
    ?assertEqual(10,length(tl:list())),    
    ?assertEqual(12,				% event manager
		 length(supervisor:
			    which_children(lights_sup))),
    {Cycling, NotCycling} =
	lists:partition(fun(#robot{spid=_N}) -> is_pid(_N) end,
			tl:list()),
    ?assertEqual({5,5}, {length(Cycling),length(NotCycling)}).

remove_all_some_switching() ->
    {Cycling, NotCycling} =
	lists:partition(fun(#robot{spid=_N}) -> is_pid(_N) end,
			tl:list()),
    ?assertEqual({5,5}, {length(Cycling),length(NotCycling)}),
    ok = tl:remove_all(),
    ?assertEqual([], tl:list()),
    ?assertEqual(2,				% event manager
		 length(supervisor:
			    which_children(lights_sup))).

kill_switch_process_then_stop_cycle() ->
    tl:add(),
    [{robot, R1, _, undefined} | _Rest] = tl:list(),
    tl:start_cycle(R1),
    [{robot, R1, _, SPid} | _Rest] = tl:list(),
    ?assertMatch([{robot, R1, _, SPid} | _Rest]
		 when is_pid(SPid),    
		      tl:list()),
    exit(SPid, stop),
    tl:stop_cycle(R1),
    ?assertMatch([{robot, R1, _, undefined} | _Rest],
		 tl:list()).

set_emergency() ->
    [{robot, R1, _, undefined} | _Rest] = tl:list(),
    ?assertEqual(normal,
		 tl:get_emergency(R1)),
    ok = tl:set_emergency(R1),
    ?assertEqual(emergency,
		 tl:get_emergency(R1)).

get_emergency() ->
    [{robot, R1, _, undefined} | _Rest] = tl:list(),
    ?assertEqual(emergency,
		 tl:get_emergency(R1)),
    tl:add(),
    [{robot, R2, _, undefined} | _Rest2] = tl:list(),
    ?assertEqual(normal,
		 tl:get_emergency(R2)),
    tl:remove(R2).

start_cycle_then_set_emergency() ->
    [{robot, R1, _, undefined} | _Rest] = tl:list(),
    ?assertEqual(emergency,
		 tl:get_emergency(R1)),
    ok = tl:start_cycle(R1),
    ?assertEqual(normal,
		 tl:get_emergency(R1)),
    ?assertMatch([{robot, R1, _, _SPid}]
		 when is_pid(_SPid),    
		      tl:list()),
    tl:set_emergency(R1),
    ?assertEqual(emergency,
		 tl:get_emergency(R1)),
    ?assertMatch([{robot, R1, _, undefined}],
		 tl:list()).
    
get_emergency_not_existing() ->
    ?assertEqual({error, does_not_exist},
		 tl:get_emergency("robot/599")).

get_lamps() ->
    [{robot, R1, _, undefined} | _Rest] = tl:list(),
    tl:start_cycle(R1),
    tl:add(),
    [{robot, R2, _, undefined} | _Rest2] = tl:list(),
    ?assertMatch(#lamps{}, tl:get_lamps(R1)),
    ?assertMatch(#lamps{}, tl:get_lamps(R2)).

get_lamps_not_existing() ->
    ?assertEqual({error, does_not_exist},
		 tl:get_lamps("robot/1908")).
