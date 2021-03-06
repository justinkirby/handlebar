-module(handlebar_render).

-export([
         process/1
         ]).

-include("handlebar.hrl").

process([]) ->
    process(["."]);
process(FilesDirs) ->

    VTs = lists:foldl(fun(P, D) ->
                              New = handlebar_walker:walk(P),
                              dict:merge(fun(_,A,B) -> A++B end, D, New)
                      end, dict:new(), FilesDirs),



    ?DEBUG("vars: ~p~n",[dict:fetch(var, VTs)]),
    ?DEBUG("templates: ~p~n",[dict:fetch(template, VTs)]),
    ?DEBUG("extras: ~p~n",[dict:fetch(xtr, VTs)]),
    Ctx = build_context(dict:fetch(var,VTs),dict:new()),
    ?DEBUG("context: ~p~n",[dict:to_list(Ctx)]),

    XtrExt = handlebar_config:get_global(xtr_ext, "xtr"),


    FullCtx = xtr_templates(dict:fetch(xtr, VTs), Ctx, XtrExt),

    ?DEBUG("full context: ~p~n",[dict:to_list(FullCtx)]),


    render_templates(dict:fetch(template, VTs), FullCtx).




build_context([],Acc) ->
    add_define(Acc);

build_context([V|Vs], Acc) ->
    case file:consult(V) of
        {error, Reason} ->
            io:format(standard_error,"Failed to consult ~p, ~p~n",[V,Reason]),
            build_context(Vs, Acc);
        {ok, Terms} ->
            New = lists:foldl(fun({Key,Value}, D) ->
                                      dict:store(Key, Value, D)
                              end, Acc, Terms),

            build_context(Vs, New)
    end.


xtr_templates([], Ctx, _Ext) -> Ctx;
xtr_templates([F|Fs], Ctx, Ext) ->
    case file:read_file(F) of
        {error, Reason} ->
            ?ERROR("failed to open extra, ~p, ~p~n",[F,Reason]);
        {ok, Data} ->
            Rendered = mustachize(Data,Ctx),
            Key = filename:basename(F,[$.|Ext]),
            xtr_templates(Fs, dict:store(list_to_atom(Key), Rendered, Ctx), Ext)
    end.





render_templates([], _Ctx) -> ok;
render_templates([T|Ts], Ctx) ->
    case file:read_file(T) of
        {error, Reason} ->
            ?ERROR("Failed to open template, ~p, ~p~n",[T,Reason]);
        {ok, Data} ->
            handlebar_out:output(T,mustachize(Data, Ctx))
    end,
    render_templates(Ts, Ctx).

add_define(Ctx) ->

    case handlebar_config:get_global(define, undefined) of
        undefined ->
            Ctx;
        Str ->
            Proplist = case consult(Str) of
                           Terms when length(Terms) > 0,
                                      is_tuple(hd(Terms)) ->
                               %% a simple {,,,}. was passed in, leave
                               %% alone since this is a proplist
                               Terms;
                           Terms when length(Terms) == 1,
                                      is_list(hd(Terms)) ->
                               %% a proplist was passed in as define,
                               %% it is now a single element list, pop
                               %% it and returne the proplist
                               hd(Terms);
                           Terms ->
                               %% no idea wtf this is!
                               ?ABORT("~p is not a {Key,Value} tuple or proplist!~n",[Terms])
                       end,
            lists:foldl(fun({Key,Value}, D) ->
                                dict:store(Key, Value, D)
                        end, Ctx, Proplist)
    end.




%%
%% Given a string or binary, parse it into a list of terms, ala file:consult/0
%%
consult(Str) when is_list(Str) ->
    consult([], Str, []);
consult(Bin) when is_binary(Bin)->
    consult([], binary_to_list(Bin), []).

consult(Cont, Str, Acc) ->
    case erl_scan:tokens(Cont, Str, 0) of
        {done, Result, Remaining} ->
            case Result of
                {ok, Tokens, _} ->
                    {ok, Term} = erl_parse:parse_term(Tokens),
                    consult([], Remaining, [Term | Acc]);
                {eof, _Other} ->
                    lists:reverse(Acc);
                {error, Info, _} ->
                    {error, Info}
            end;
        {more, Cont1} ->
            consult(Cont1, eof, Acc)
    end.


mustachize(Data, Ctx) ->
    ReOpts = [global, {return, list}],
    Str0 = re:replace(Data, "\\\\", "\\\\\\", ReOpts),
    Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),
    mustache:render(Str1, Ctx).
