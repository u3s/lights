-module(tl_tests).
-include("../include/tl.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(ROBOT_1, "robot/1").
-define(ROBOT_2, "robot/2").
-define(ROBOT_3, "robot/3").
-define(ROBOT_4, "robot/4").

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
    application:start(lights), ok.

cleanup(_) ->
    application:stop(lights).

add() ->
    tl:add(),
    ?assertMatch([{robot, R1,_,_}]
		 when R1 == ?ROBOT_1,
		      tl:list()).
remove() ->
    ?assertMatch([{robot, R1,_,_}]
		 when R1 == ?ROBOT_1,
		      tl:list()),
    tl:remove(?ROBOT_1),
    ?assertEqual([], tl:list()).

remove_not_existing() ->
    ?assertMatch([], tl:list()),
    ?assertMatch({error, does_not_exist},
		 tl:remove(?ROBOT_1)).
lookup_ok() ->
    ?assertEqual({ok, ?ROBOT_1}, tl:lookup(?ROBOT_1)).

lookup_nok() ->
    ?assertEqual({{error, not_found}, ?ROBOT_2}, tl:lookup(?ROBOT_2)).

list() ->
    tl:add(),
    ?assertMatch([{robot, R2, RPid2, undefined},
		  {robot, R1, RPid1, undefined}]
		 when R2 == ?ROBOT_2 andalso
		      R1 == ?ROBOT_1 andalso
		      is_pid(RPid2) andalso
		      is_pid(RPid1),
		 tl:list()),
    tl:remove(?ROBOT_2).

list_empty() ->
    ?assertEqual([], tl:list()).

start_cycle() ->
    ok = tl:start_cycle(?ROBOT_1),
    ?assertMatch([{robot, R1, _, SPid}]
		when R1 == ?ROBOT_1 andalso
		     is_pid(SPid),
		     tl:list()).

stop_cycle() ->
    ?assertMatch([{robot, R1, _, SPid}]
		when R1 == ?ROBOT_1 andalso
		     is_pid(SPid),
		     tl:list()),
    ok = tl:stop_cycle(?ROBOT_1),
    ?assertMatch([{robot, R1, _, undefined}]
		 when R1 == ?ROBOT_1,
		      tl:list()).

try_remove_cycling() ->
    ?assertMatch([{robot, R1, _, SPid}]
		when R1 == ?ROBOT_1 andalso
		     is_pid(SPid),
		     tl:list()),
    ?assertEqual({warning, robot_active}, tl:remove(?ROBOT_1)),
    ?assertMatch([{robot, R1, _, SPid}]
		when R1 == ?ROBOT_1 andalso
		     is_pid(SPid),
		     tl:list()).

stop_cycle_not_cycling() ->
    ?assertMatch([{robot, R1, _, undefined}]
		 when R1 == ?ROBOT_1,
		      tl:list()),
    ok = tl:stop_cycle(?ROBOT_1),
    ?assertMatch([{robot, R1, _, undefined}]
		 when R1 == ?ROBOT_1,
		      tl:list()).

start_cycle_already_cycling() ->
    ?assertMatch([{robot, R1, _, SPid}]
		 when R1 == ?ROBOT_1 andalso
		      is_pid(SPid),
		      tl:list()),
    ok = tl:start_cycle(?ROBOT_1),
    ?assertMatch([{robot, R1, _, SPid}]
		 when R1 == ?ROBOT_1 andalso
		      is_pid(SPid),
		      tl:list()).    

start_cycle_not_existing() ->
    ?assertEqual([], tl:list()),
    ok = tl:start_cycle(?ROBOT_1),
    ?assertEqual([], tl:list()).

stop_cycle_not_existing_robot() ->
    ?assertEqual([], tl:list()),
    ok = tl:stop_cycle(?ROBOT_1),
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
    ?assertEqual(11,
		 length(supervisor:
			    which_children(lights_sup))),
    ok = tl:remove_all(),
    ?assertEqual([], tl:list()),
    ?assertEqual(1,
		 length(supervisor:
			    which_children(lights_sup))).    

add_10_start_cycle_5() ->
    Run =  fun Loop(0) ->
		   ok;
	       Loop(N) when N rem 2 =:= 0 ->
		   tl:add(),
		   Loop(N-1);
	       Loop(N) ->
		   tl:add(),
		   tl:start_cycle("robot/"++integer_to_list(10-N)),
		   Loop(N-1)
	   end,
    Run(10),
    ?assertEqual(10,length(tl:list())),    
    ?assertEqual(11,
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
    ?assertEqual(1,
		 length(supervisor:
			    which_children(lights_sup))).

kill_switch_process_then_stop_cycle() ->
    tl:add(),
    tl:start_cycle(?ROBOT_1),
    ?assertMatch([{robot, R1, _, SPid}]
		 when R1 == ?ROBOT_1 andalso
		      is_pid(SPid),    
		      tl:list()),
    [#robot{spid=SPid}] = tl:list(),
    exit(SPid, stop),
    tl:stop_cycle(?ROBOT_1),
    ?assertMatch([{robot, R1, _, undefined}]
		 when R1 == ?ROBOT_1,   
		      tl:list()).

set_emergency() ->
    ok = tl:set_emergency(?ROBOT_1).

get_emergency() ->
    ?assertEqual(emergency,
		 tl:get_emergency(?ROBOT_1)),
    tl:add(),
    ?assertEqual(normal,
		 tl:get_emergency(?ROBOT_2)),
    tl:remove(?ROBOT_2).

start_cycle_then_set_emergency() ->
    ?assertEqual(emergency,
		 tl:get_emergency(?ROBOT_1)),
    ok = tl:start_cycle(?ROBOT_1),
    ?assertEqual(normal,
		 tl:get_emergency(?ROBOT_1)),
    ?assertMatch([{robot, R1, _, SPid}]
		 when R1 == ?ROBOT_1 andalso
		      is_pid(SPid),    
		      tl:list()),
    tl:set_emergency(?ROBOT_1),
    ?assertEqual(emergency,
		 tl:get_emergency(?ROBOT_1)),
    ?assertMatch([{robot, R1, _, undefined}]
		 when R1 == ?ROBOT_1,   
		      tl:list()).
    
get_emergency_not_existing() ->
    ?assertEqual({error, does_not_exist},
		 tl:get_emergency(?ROBOT_3)).

get_lamps() ->
    tl:start_cycle(?ROBOT_1),
    tl:add(),
    ?assertMatch(#lamps{}, tl:get_lamps(?ROBOT_1)),
    ?assertMatch(#lamps{}, tl:get_lamps(?ROBOT_2)).

get_lamps_not_existing() ->
    ?assertEqual({error, does_not_exist},
		 tl:get_lamps(?ROBOT_3)).
