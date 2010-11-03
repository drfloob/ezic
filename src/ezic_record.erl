-module(ezic_record).
-include("include/ezic.hrl").
-export([rule/1,zone/1,link/1]).


rule([Name,From,To,Type,In,On,At,Save,Letters]) ->
    {ok, #rule{
       name=Name, from=From, to=To, type=Type,
       in=In, on=On, at=At, save=Save, letters=Letters
      }}.

zone([Name,GmtOff,Rules,Format | UntilTokens]) ->
    Until = parse_until(UntilTokens),
    {ok, #zone{
       name=Name, gmtoff=GmtOff, rules=Rules,
       format=Format, until=Until
      }}.
    
link([From,To]) ->
    {ok, #link{from=From, to=To}}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_until([]) ->
    current;
parse_until([YS]) ->
    Y= list_to_integer(YS),
    {Y,1,1};
parse_until([YS,MS]) ->
    Y= list_to_integer(YS),
    M=month_to_num(MS),
    {Y,M,1};
parse_until([YS,MS,DS]) ->
    Y= list_to_integer(YS),
    M=month_to_num(MS),
    D= list_to_integer(DS),
    {Y,M,D};
% @todo broken: time flags must be accounted for
parse_until([YS,MS,DS,TS]) ->
    Y= list_to_integer(YS),
    M=month_to_num(MS),
    D= list_to_integer(DS),
    TimeTokens=string:tokens(TS, ":"),
    {{Y,M,D},parse_time(TimeTokens)}.



parse_time("-") ->
    {0,0,0};
parse_time([HS]) ->
    H= list_to_integer(HS),
    {H,0,0};
parse_time([HS,MS]) ->
    H= list_to_integer(HS),
    M= list_to_integer(MS),
    {H,M,0};
parse_time([HS,MS,SS]) ->
    H= list_to_integer(HS),
    M= list_to_integer(MS),
    S= list_to_integer(SS),
    {H,M,S}.

    



month_to_num("Jan") -> 1;
month_to_num("Feb") -> 2;
month_to_num("Mar") -> 3;
month_to_num("Apr") -> 4;
month_to_num("May") -> 5;
month_to_num("Jun") -> 6;
month_to_num("Jul") -> 7;
month_to_num("Aug") -> 8;
month_to_num("Sep") -> 9;
month_to_num("Oct") -> 10;
month_to_num("Nov") -> 11;
month_to_num("Dec") -> 12.
