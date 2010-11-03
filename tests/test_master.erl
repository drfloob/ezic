-module(test_master).
-export([start/0]).

start() ->
    ezic_record:test().
