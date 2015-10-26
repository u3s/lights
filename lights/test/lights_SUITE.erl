-module(lights_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, tc01_eunit/1, tc02_simple_crossroad/1, groups/0, init_per_group/2, end_per_group/2]).

-define(TIME_R, 10000).
-define(TIME_RY, 2000).
-define(TIME_G, 10000).
-define(TIME_Y, 2000).

-define(LAMPS_RED, {lamps, on, off, off}).
-define(LAMPS_GREEN, {lamps, off, off, on}).
-define(LAMPS_RED_YELLOW, {lamps, on, on, off}).
-define(LAMPS_YELLOW, {lamps, off, on, off}).

all() -> [{group, eunit}, {group, integration}].

groups() -> [{eunit, [], [tc01_eunit]}, 
			 {integration, [], [tc02_simple_crossroad]}].

tc01_eunit(_Config) ->
    ok = eunit:test(tl_tests, [verbose]).


init_per_group(integration, Config) ->
	{current_function, {_, FunctionName, _}} = process_info(self(), current_function),
    ok = application:start(lights),
    log("lights application started", FunctionName),
	Config;

init_per_group(_, _Config) ->
	_Config.

end_per_group(integration, Config) ->
	{current_function, {_, FunctionName, _}} = process_info(self(), current_function),
	tl:remove_all(),
	log("robots removed", FunctionName),
    ok = application:stop(lights),
    log("lights application stopped", FunctionName),
	Config;
end_per_group(_, _Config) ->
	ok.

tc02_simple_crossroad(_Config) ->
	{current_function, {_, FunctionName, _}} = process_info(self(), current_function),
    tl:add(),
    [{robot, R1, _, undefined} | _] = tl:list(),
    tl:add(),
    [{robot, R2, _, undefined} | _] = tl:list(),

    tl:add(),
    [{robot, R3, _, undefined} | _] = tl:list(),
    tl:add(),
    [{robot, R4, _, undefined} | _] = tl:list(),
	log("robots added", FunctionName),
	4 = length(tl:list()),

    NSDirection = [R1, R2],
    WEDirection = [R3, R4],
	log("NS and WE robot lists created", FunctionName),
    
	[ok = tl:start_cycle(R) || R <- NSDirection],
	log("NS direction robots started", FunctionName),

	ct:sleep(?TIME_RY + ?TIME_G + 1),

	[ok = tl:start_cycle(R) || R <- WEDirection],
	log("WE direction robots started", FunctionName),

	log("NS direction state", FunctionName),
	print_lamps_for_direction(NSDirection),
	[?LAMPS_YELLOW = tl:get_lamps(R) || R <- NSDirection],

	log("WE direction state", FunctionName),
	print_lamps_for_direction(WEDirection),
	[?LAMPS_RED_YELLOW = tl:get_lamps(R) || R <- WEDirection],

	ct:sleep(?TIME_RY),

	log("NS direction state", FunctionName),
	print_lamps_for_direction(NSDirection),
	[?LAMPS_RED = tl:get_lamps(R) || R <- NSDirection],

	log("WE direction state", FunctionName),
	print_lamps_for_direction(WEDirection),
	[?LAMPS_GREEN = tl:get_lamps(R) || R <- WEDirection],

	log("Simulating log working period", FunctionName),
	ct:sleep((?TIME_R + ?TIME_RY)),

	log("NS direction state", FunctionName),
	print_lamps_for_direction(NSDirection),
	[?LAMPS_GREEN = tl:get_lamps(R) || R <- NSDirection],

	log("WE direction state", FunctionName),
	print_lamps_for_direction(WEDirection),
	[?LAMPS_RED = tl:get_lamps(R) || R <- WEDirection],

	[ok = tl:stop_cycle(R) || R <- NSDirection],
	log("NS direction robots stopped", FunctionName),

	[ok = tl:stop_cycle(R) || R <- WEDirection],
	log("WE direction robots stopped", FunctionName),
	
	ok.

print_lamps_for_direction(RobotList) ->
	{current_function, {_, FunctionName, _}} = process_info(self(), current_function),
	[print_lamps_for_robot(R) || R <- RobotList],
	ok.

print_lamps_for_robot(Robot) ->
	{current_function, {_, FunctionName, _}} = process_info(self(), current_function),
	Lamps = tl:get_lamps(Robot),

	LampsColor = case Lamps of
					?LAMPS_RED -> "RED";
					?LAMPS_RED_YELLOW -> "RED_YELLOW";
					?LAMPS_GREEN -> "GREEN";
					?LAMPS_YELLOW -> "YELLOW"
				end,

	log(LampsColor, FunctionName),
	ok.



log(Msg,CallerName) ->	
	PrevValue = case get(CallerName) of
					undefined ->
						0;
					OldVal ->
						OldVal
				end,
	put(CallerName, PrevValue+1),
	ct:pal("[~p #~p] ~p", [CallerName, PrevValue+1, Msg]),
	ok.