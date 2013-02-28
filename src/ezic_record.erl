-module(ezic_record).
-include("include/ezic.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([
	 link/1
	 , leap/1
	 , separate/1
	]).



link([From,To]) ->
    {ok, #link{from=From, to=To}}.


% @todo figure out what must be done with leap seconds
leap([YearS, MonthS, DayS, TimeS, Corr, RS]) ->
    Y= list_to_integer(YearS),
    M= ezic_date:month_to_num(MonthS),
    D= list_to_integer(DayS),
    Time= ezic_parse:time_val(TimeS),
    {ok, #leap{datetime={{Y,M,D}, Time}, corr=Corr, rs=RS}}.




separate(Records) ->
    Zones= [Z || Z=#zone{} <- Records],
    Rules= [Z || Z=#rule{} <- Records],
    Leaps= [Z || Z=#leap{} <- Records],
    Links= [Z || Z=#link{} <- Records],
    {ok, Zones, Rules, Leaps, Links}.


