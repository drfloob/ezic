-module(erldev).
%% Contains development-related functions.

%-compile([export_all]).
-export([make_app/1]).




%% "Compiles" a *.app.src file into a .app file, and places it in
%% the appropriate ebin folder
make_app([AppDirAtom]) ->
    %%@todo: module name conflicts?
    AppDir = atom_to_list(AppDirAtom),
    Modules = get_modules(AppDir),
    AppSrc = try   get_app_src(AppDir)
	     catch error:{badmatch, _} ->
%		     error_logger:error_msg("Couldn't get AppSrcFile for ~p~n", [AppDir]),
		     erlang:halt()
	     end,
    {application, Name, Opts} = AppSrc,
    NewOpts = [ {modules, Modules} | Opts ],
    AppFile = filename:join([AppDir, "ebin", atom_to_list(Name) ++ ".app"]),
    ok = file:write_file(AppFile, io_lib:format("~p.~n", [{application, Name, NewOpts}])),
    error_logger:info_msg("Wrote .app contents to: ~p~n", [AppFile]),
    ok.



%% Returns a list of module names of all *.erl files in a given Application
%% folder (assuming OTP directory structure)
get_modules(AppDir) ->
    lists:map(
      fun(X)-> filename:basename(X, ".erl") end,
      filelib:wildcard(
	filename:join([AppDir, "src", "*.erl"])
       )
     ).



%% Converts the contsnts of the *.app.src file into erlang terms
get_app_src(AppDir) ->
    [AppSrcFile] = filelib:wildcard(filename:join([AppDir, "src", "*.app.src"])),
    {ok, [AppSrc]} = file:consult(AppSrcFile),
    %error_logger:info_msg("AppSrc: ~p~n", [AppSrc]),
    AppSrc.

