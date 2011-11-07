-module(handlebar_walker).

-export([
         walk/1
         ]).

-include("handlebar.hrl").


walk(Path) ->
    Var = handlebar_config:get_global(vars_ext,"vars"),
    Tmp = handlebar_config:get_global(template_ext, "src"),


    Data = extract_from_path(Path, Var, Tmp),

    case handlebar_config:get_global(recurse,"0") of
        "0" -> Data;
        "1" ->
            Dirs = proplists:get_all_values(dir,Data),
            Data0 = proplists:delete(dir,Data),
            walk(Dirs, Var, Tmp, Data0)
    end.



walk([], _Var, _Tmp, Acc) -> Acc;

walk([SubDir|Dirs], Var, Tmp, Acc) ->
    Acc0 = extract_from_path(SubDir, Var, Tmp, Acc),
    Subs = proplists:get_all_values(dir, Acc0),
    Acc1 = proplists:delete(dir, Acc0),
    Acc2 = walk(Subs, Var, Tmp, Acc1),
    walk(Dirs, Var, Tmp, Acc2).




extract_from_path(Path, Var, Tmp) ->
    extract_from_path(Path, Var, Tmp, []).
extract_from_path(Path, Var, Tmp, Data) ->
    case filelib:is_dir(Path) of
        false ->
            DataVar = if_match_add(Path, var, Var, Data),
            if_match_add(Path, template, Tmp, DataVar);
        true ->
            {ok, Paths} = file:list_dir(Path),
            SubPaths = [filename:join(Path,P) || P <- Paths],
            cat_paths(SubPaths, Var, Tmp, Data)
    end.



cat_paths([], _Var,_Tmp, Acc) ->
    Acc;
cat_paths([P|Ps], Var, Tmp, Data) ->
    Dirs = if_isdir_add(P,Data),
    Vars = if_match_add(P, var, Var, Dirs),
    Tmps = if_match_add(P, template, Tmp, Vars),
    cat_paths(Ps, Var, Tmp, Tmps).




if_isdir_add(Path,Dirs) ->
    case filelib:is_dir(Path) of
        false ->
            Dirs;
        true ->
            [{dir,Path}|Dirs]
    end.

if_match_add(Path, Key, Ext, Vars) ->
    case match_ext(Path,Ext) of
        false ->
            Vars;
        true ->
            [{Key,Path}|Vars]
    end.


match_ext(Path, Ext) ->
    filename:extension(Path) == [$.|Ext].
