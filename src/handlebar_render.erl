-module(handlebar_render).

-export([
         process/1
         ]).

-include("handlebar.hrl").
process([]) ->
    process(["."]);
process(FilesDirs) ->


    VTs = lists:flatten([handlebar_walker:walk(Path) || Path <- FilesDirs]),
    ?DEBUG("vars: ~p~n",[proplists:get_all_values(var, VTs)]),
    ?DEBUG("templates: ~p~n",[proplists:get_all_values(template, VTs)]),
    Ctx = build_context(VTs),
    ?DEBUG("context: ~p~n",[dict:to_list(Ctx)]),

    render_templates(VTs, Ctx).






build_context(VTs) ->
    Vars = proplists:get_all_values(var, VTs),
    build_context(lists:usort(Vars), []).

build_context([],Acc) ->
    %% this is where the dict blows away duplicates and uses the last
    %% key
    dict:from_list(Acc);
build_context([V|Vs], Acc) ->
    case file:consult(V) of
        {error, Reason} ->
            io:format("Failed to consult ~p, ~p~n",[V,Reason]),
            build_context(Vs, Acc);
        {ok, Terms} ->
            %% this is where we have duplicate keys in the proplist.
            build_context(Vs, Acc++Terms)
    end.


render_templates(VTs,Ctx) ->
    Tmps = proplists:get_all_values(template, VTs),
    render_template(Tmps, Ctx).

render_template([], _Ctx) -> ok;
render_template([T|Ts], Ctx) ->
    case file:read_file(T) of
        {error, Reason} ->
            ?ERROR("Failed to open template, ~p, ~p~n",[T,Reason]);
        {ok, Data} ->
            Output = mustache:render(binary_to_list(Data), Ctx),
            handlebar_out:output(T,Output)
    end,
    render_template(Ts, Ctx).
