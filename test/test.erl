-module(test).
-export([all/0]).
-include_lib("eunit/include/eunit.hrl").

all() ->
    eunit:test(ezic_record),
    eunit:test(ezic_date).
