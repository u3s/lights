-module(lights_SUITE).
-include_lib("common_test/include/ct.hrl").
%-include("../include/tl.hrl").
-export([all/0,tc_eunit/1]).

all() -> [tc_eunit].

tc_eunit(_Config) ->
    ok = eunit:test(tl_tests, [verbose]).