-module(tl_tests).
-include_lib("eunit/include/eunit.hrl").
-define(ROBOT_1, "robot/1").
-define(ROBOT_2, "robot/2").
-define(ROBOT_3, "robot/3").
-define(ROBOT_4, "robot/4").

add_one_robot_test() ->
    application:start(lights),
    tl:add(),
    ?assertMatch([{R1,_,worker,[tl_fsm]},
		  {tl,_,worker,[tl]}]
		 when R1 == ?ROBOT_1,
		      tl:list()),
    application:stop(lights).
add_4_robots_remove_1_add_1_test() ->
    application:start(lights),
    tl:add(),
    tl:add(),
    tl:add(),
    tl:add(),
    ?assertMatch([{R4,_,worker,[tl_fsm]},
		  {R3,_,worker,[tl_fsm]},
		  {R2,_,worker,[tl_fsm]},
		  {R1,_,worker,[tl_fsm]},
		  {tl,_,worker,[tl]}]
		 when R1 == ?ROBOT_1 andalso
		      R2 == ?ROBOT_2 andalso
		      R3 == ?ROBOT_3 andalso
		      R4 == ?ROBOT_4,
		      tl:list()),
    tl:remove(?ROBOT_3),
    ?assertMatch([{R4,_,worker,[tl_fsm]},
		  {R2,_,worker,[tl_fsm]},
		  {R1,_,worker,[tl_fsm]},
		  {tl,_,worker,[tl]}]
		 when R1 == ?ROBOT_1 andalso
		      R2 == ?ROBOT_2 andalso
		      R4 == ?ROBOT_4,
		      tl:list()),
    application:stop(lights).
lookup_ok_test() ->
    application:start(lights),
    tl:add(),
    tl:list(),
    ?assertEqual({ok, ?ROBOT_1}, tl:lookup(?ROBOT_1)),
    application:stop(lights).

lookup_nok_test() ->
    application:start(lights),
    tl:add(),
    tl:list(),
    ?assertEqual({nok, ?ROBOT_2}, tl:lookup(?ROBOT_2)),
    supervisor:terminate_child(lights_sup, tl),
    supervisor:delete_child(lights_sup, tl),
    application:stop(lights).

start_cycle_test() ->
    application:start(lights),
    tl:add(),
    ok = tl:start_cycle("robot/1"),
    application:stop(lights).
    
    
