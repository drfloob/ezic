-module(ezic).

-export([load/1, time2time/3]).
-export([dev_start/0]).


load(Folder) ->
    ezic_loader:load(Folder).



time2time(_DateTime, _FromTimeZone, _ToTimeZone) ->
    void.




dev_start() ->
    ezic:load(filename:join("priv","tzdata")).
