-module(ezic).
-export([load/1, time2time/3]).


load(Folder) ->
    ezic_loader:load(Folder).


time2time(_DateTime, _FromTimeZone, _ToTimeZone) ->
    void.
