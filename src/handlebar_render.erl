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
    Ctx = build_context(dict:fetch(var,VTs),dict:new()),
    ?DEBUG("context: ~p~n",[dict:to_list(Ctx)]),

    render_templates(dict:fetch(template, VTs), Ctx).




build_context([],Acc) ->
    add_define(Acc);

build_context([V|Vs], Acc) ->
    case file:consult(V) of
        {error, Reason} ->
            io:format("Failed to consult ~p, ~p~n",[V,Reason]),
            build_context(Vs, Acc);
        {ok, Terms} ->
            New = lists:foldl(fun({Key,Value}, D) ->
                                      dict:store(Key, Value, D)
                              end, Acc, Terms),

            build_context(Vs, New)
    end.


render_templates([], _Ctx) -> ok;
render_templates([T|Ts], Ctx) ->
    case file:read_file(T) of
        {error, Reason} ->
            ?ERROR("Failed to open template, ~p, ~p~n",[T,Reason]);
        {ok, Data} ->
            ReOpts = [global, {return, list}],
            Str0 = re:replace(Data, "\\\\", "\\\\\\", ReOpts),
            Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),
            Output = mustache:render(Str1, Ctx),
            handlebar_out:output(T,Output)
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
