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
    flatten(Records).
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
    CPos= string:chr(Line2, $#),
    case CPos of
	0 -> Line2;
	_ -> string:sub_string(Line2, 1, CPos-1)
    end.



build_record("Rule", Data,_) ->
    ezic_record:rule(Data);
build_record("Zone", Data,_) ->
    ezic_record:zone(Data);
build_record("Link", Data,_) ->
    ezic_record:link(Data);
build_record(GmtOff, Data, {"Zone", PrevName}) ->
    ezic_record:zone([PrevName,GmtOff|Data]);

build_record(Type, Data, PT) ->
    {error, {badLine, Type, Data, PT}}.


% converts tzdata records to flattened records
% returns list of #flatzone{}
flatten(Records) when is_list(Records) ->
    void.
