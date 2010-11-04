-module(ezic_rule).
-include("include/ezic.hrl").


-export([]).


% returns a list of all {datetime,offset} sets that must be
% projected for this Rule.
project(Rule=#rule{from=F, to=only}) ->
    not_done;
project(Rule=#rule{from=F, to=T}) ->
    Dates= lists:map(fun(Y)-> ezic_date:for(Rule, Y)  end, lists:seq(F, T)).
