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




build_context([],Acc) -> Acc;

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
            Output = mustache:render(binary_to_list(Data), Ctx),
            handlebar_out:output(T,Output)
    end,
    render_templates(Ts, Ctx).
