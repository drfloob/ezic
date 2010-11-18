-module(ezic_loader).
-include("include/ezic.hrl").

-export([load/0, load/1]).
-define(TZDIR, filename:join("priv", "tzdata")).



load() ->
    load(?TZDIR).

%% returns all records, parsed from the tzdata files.
load(File) ->
    {ok, Records} = 
	case filelib:is_dir(File) of
	    true -> {ok, parse_dir(File)};
	    false -> erlang:error(not_done)

		%% case filelib:is_regular(File) of
		%%     true -> {ok, parse_file(File)};
		%%     false -> erlang:error(badFile, File)
		%% end
	end,

    Records.

    %% {ok, Zones, Rules, Leaps, Links} = ezic_record:separate(Records),

    %% ezic_db:wipe(),
    %% ezic_db:init(),

    %% ezic_db:insert_all(Zones),
    %% ezic_db:insert_all(Rules),
    %% ezic_db:insert_all(Leaps),
    %% ezic_db:insert_all(Links).
    




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% returns a list of tzdata records for all files in folder
% note: assumes every file in the folder is a tzdata file
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
    NewRecords= case length(StrLine) > 0 of
	true -> [parse_to_record(StrLine, File, Records) | Records];
	false -> Records
    end,
    parse_lines(file:read_line(File), File, NewRecords).



% the Line has data, so we parse it, build a record, and return it
parse_to_record(Line, _, Records) ->
    [Type | Data] = string:tokens(Line, " \t"),
    {ok, PrevType, PrevName} = prev_rec_type(Records),
    {ok, Record} = build_record(Type, Data, {PrevType, PrevName}),
    Record.




% retrieves the previous record type, in case Zone continuations occur (multiline)
prev_rec_type([]) ->
    {ok, null, null};
prev_rec_type([#zone{name=Name}|_]) ->
    {ok, "Zone", Name};
prev_rec_type(List) when is_list(List) ->
    {ok, void, void}.




clean_line(Line) ->
    %% remove spaces, newlines, and tabs
    Line1= string:strip(Line),
    Line2= string:strip(Line1, both, $\n),
    Line3= string:strip(Line2, both, $\t),
    
    %% remove comments and return
    FinalLine=Line3,
    CPos= string:chr(FinalLine, $#),
    case CPos of
	0 -> FinalLine;
	_ -> string:sub_string(FinalLine, 1, CPos-1)
    end.



build_record("Rule", Data,_) ->
    ezic_rule:parse(Data);
build_record("Zone", Data,_) ->
    ezic_zone:parse(Data);
build_record("Link", Data,_) ->
    ezic_record:link(Data);
build_record("Leap", Data,_) ->
    ezic_record:leap(Data);
build_record(GmtOff, Data, {"Zone", PrevName}) ->
    ezic_zone:parse([PrevName,GmtOff|Data]);

build_record(Type, Data, PT) ->
    erlang:error(badLine, {Type, Data, PT}).




