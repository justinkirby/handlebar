-module(handlebar_walker).

-export([
         walk/1
         ]).

-include("handlebar.hrl").

walk(Path) ->
    Var = handlebar_config:get_global(vars_ext,"vars"),
    Tmp = handlebar_config:get_global(template_ext, "src"),
    Recurse = case handlebar_config:get_global(recurse, "0") of
                  "0" -> false;
                  _ -> true
              end,

    Anchor = case handlebar_config:get_global(anchor) of
                 undefined ->
                     %% we set the anchor to something if and only if
                     %% the paths are relative., i.e. the first char
                     %% of the path is /
                     case hd(Path) of
                         $/ -> [];
                         _ ->
                             {ok, Dir} = file:get_cwd(),
                             Dir
                     end;
                 Dir -> Dir
             end,

    case filelib:is_regular(filename:join(Anchor,Path)) of
        true ->
            handle_file(filename:join(Anchor, Path), Var, Tmp);
        false ->
            handle_dir(Anchor, Path, Var, Tmp, Recurse)
    end.

handle_file(Path, Var, Tmp) ->
    ?DEBUG("~p ~p~n",[handle_file, Path]),

    VarExt = [$.|Var],
    TmpExt = [$.|Tmp],
    case filename:extension(Path) of
        VarExt ->
            dict:from_list([{var,[Path]},{template,[]}]);
        TmpExt ->
            dict:from_list([{var,[]},{template,[Path]}]);
        _ ->
            dict:from_list([{var,[]},{template,[]}])
    end.


handle_dir(Anchor, Path, Var, Tmp, Recurse) ->
    ?DEBUG("~p ~p/~p:~p~n",[handle_dir, Anchor, Path,Recurse]),


    %% what style of nav is specified?
    %% - tree
    %% - branch
    %% - term

    case handlebar_config:get_global(navigate,"tree") of
        "tree" ->
            RootedPath = filename:join(Anchor, Path),
            tree_walk(RootedPath, Var, Tmp, Recurse);
        "branch" ->
            branch_walk(Anchor, Path, Var, Tmp);
        N  ->
            ?ABORT("~p is an invalid walking method, see help~n",[N])
    end.

tree_walk(Path, Var, Tmp, Recurse) ->

    %% instead of doing diff extensions, could also do a single regex
    %% of ".*\\.(Var|Tmp)$" and then have a complicated accumulator
    %% fun to create a dict of var and template
    %% paths... ick... dealing with inefficiency of multiple dir walks
    %% for simpler code.
    VarRe = ".*\\."++Var++"$",
    TmpRe = ".*\\."++Tmp++"$",
    Vars = filelib:fold_files(Path, VarRe, Recurse,
                              fun(F,A) -> [F|A] end,
                              []),
    Tmps = filelib:fold_files(Path, TmpRe, Recurse,
                              fun(F,A) -> [F|A] end,
                              []),

    %%see comment on sort filename
    VarsSort = lists:sort(fun sort_filename/2, Vars),
    TmpsSort = lists:sort(fun sort_filename/2, Tmps),

    dict:from_list([{var, VarsSort},{template, TmpsSort}]).

branch_walk(Anchor, Path, Var, Tmp) ->

    Paths = branch_paths(Anchor, string:tokens(Path,"/"),[]),

    %% loop over the paths in order and do a nonrecursive tree
    %% walk... then we sort on final result... man this is getting
    %% really inefficient. I hope no one uses this in performance
    %% critical paths
    lists:foldl(fun(P, D) ->
                        Walked = tree_walk(P, Var, Tmp, false),
                        dict:merge(fun(_,A,B) -> A++B end,
                                   D,Walked)
                end,
                dict:new(), Paths).



%% turn /a/b, x/y/z to [/a/b/x, /a/b/x/y, /a/b/x/y/z]
branch_paths(_Anchor, [], Acc) -> lists:reverse(Acc);
branch_paths(Anchor, [P|Path], Acc) ->
    New = filename:join(Anchor, P),
    branch_paths(New, Path, [New|Acc]).


%% this is to solve the mis order of /a/a/a.foo and /a/b.foo normal
%% alpha sort would put them in that order, we really want /a/b.foo
%% then /a/a/a.foo This fits more with the 'directory sort' intuition
%% (at least my intuition)
sort_filename(A,B) ->
    %% if the dir is == then compare filename
    %% otherwise use the dirname
    Adir = filename:dirname(A),
    Bdir = filename:dirname(B),
    case Adir of
        Bdir ->
            %% they are the same, compare filenames
            filename:basename(A) =< filename:basename(B);
        _ ->
            %% dirs are not the same, then use the dir to compare
            Adir =< Bdir
    end.



