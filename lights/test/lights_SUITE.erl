-module(lights_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, tc01_eunit/1, tc02_simple_crossroad/1, groups/0, init_per_group/2, end_per_group/2]).

all() -> [{group, eunit}, {group, integration}].

groups() -> [{eunit, [], [tc01_eunit]}, 
			 {integration, [], [tc02_simple_crossroad]}].

tc01_eunit(_Config) ->
    ok = eunit:test(tl_tests, [verbose]).


init_per_group(integration, Config) ->
    ok = application:start(lights),
    ct:pal(">>> lights application started"),
    tl:add(),
    tl:add(),
    tl:add(),
    tl:add(),
    ct:pal(">>> robots added"),
	Config;

init_per_group(_, _Config) ->
	_Config.

end_per_group(integration, Config) ->
	tl:remove_all(),
	ct:pal(">>> robots removed"),
    ok = application:stop(lights),
    ct:pal(">>> lights application stopped"),
	Config;
end_per_group(_, Config) ->
	ok.

tc02_simple_crossroad(_Config) ->
	ok.
