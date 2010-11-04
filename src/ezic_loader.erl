-module(ezic_loader).
-include("include/ezic.hrl").

-export([load/1]).


load(File) ->
    {ok, Records} = 
	case filelib:is_dir(File) of
	    true -> {ok, parse_dir(File)};
	    false -> case filelib:is_regular(File) of
			 true -> {ok, parse_file(File)};
			 false -> {error, {badFile, File}}
		     end
	end,
    {ok, Zones, Rules, Leaps, Links} = ezic_compile:separate(Records),
    ezic_compile:flatten(Zones, Rules).
    % @todo store in mnesia





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% returns a list of tzdata records for all files in folder
% note: assumes every file in the folder is a tzdata file
% this method does not recurse.
parse_dir(Folder) ->
    {ok, Files} = file:list_dir(Folder),
    lists:flatten([ parse_file(filename:join(Folder,File)) || File <- Files ]).



% returns a list of tzdata records from file
parse_file(File) ->
    {ok, FD} = file:open(File, [read]),
    parse_lines(file:read_line(FD), FD, []).



parse_lines(eof, _, Records) ->
    Records;
parse_lines({ok, Line}, File, Records) ->
    StrLine= clean_line(Line),
    case length(StrLine) > 0 of
	false -> parse_lines(file:read_line(File), File, Records);
	true -> parse_lines_2(StrLine, File, Records)
    end.


parse_lines_2(Line, File, Records) ->
    %?debug("Line: ~p", [Line]),
    [Type | Data] = string:tokens(Line, " \t"),
    {ok, PrevType, PrevName} = prev_rec_type(Records),
    {ok, Record} = build_record(Type, Data, {PrevType, PrevName}),
    parse_lines(file:read_line(File), File, [Record|Records]).


prev_rec_type([]) ->
    {ok, null, null};
prev_rec_type([#zone{name=Name}|_]) ->
    {ok, "Zone", Name};
prev_rec_type(List) when is_list(List) ->
    {ok, void, void}.

clean_line(Line) ->
    Line1= string:strip(Line),
    Line2= string:strip(Line1, both, $\n),
    Line3= string:strip(Line2, both, $\t),
    
    FinalLine=Line3,
    CPos= string:chr(FinalLine, $#),
    case CPos of
	0 -> FinalLine;
	_ -> string:sub_string(FinalLine, 1, CPos-1)
    end.



build_record("Rule", Data,_) ->
    ezic_record:rule(Data);
build_record("Zone", Data,_) ->
    ezic_record:zone(Data);
build_record("Link", Data,_) ->
    ezic_record:link(Data);
build_record(GmtOff, Data, {"Zone", PrevName}) ->
    ezic_record:zone([PrevName,GmtOff|Data]);
build_record("Leap", Data,_) ->
    ezic_record:leap(Data);

build_record(Type, Data, PT) ->
    {error, {badLine, Type, Data, PT}}.

