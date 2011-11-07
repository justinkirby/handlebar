%% note that a *lot* of this was borrowed from rebar

-module(handlebar).



-export([
         main/1
        ]).


-include("handlebar.hrl").


main(Args) ->
    case catch(run(Args)) of
        ok ->
            ok;
        {error, failed} ->
            halt(1);
        Error ->
            io:format("Uncaught error: ~p~n",[Error]),
            halt(1)
    end.



run(Raw) ->
    ok = application:load(handlebar),
    run_aux(parse_args(Raw)).

run_aux(["help"]) ->
    help(),
    ok;
run_aux(["version"]) ->
    version(),
    ok;
run_aux(Args) ->
    handlebar_log:init(),
    handlebar_render:process(Args),
    ok.


parse_args(Args) ->
    OptSpecList = option_spec_list(),

    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            %% Check options and maybe halt
            false = show_info_maybe_halt(Options),

            set_global_flag(Options, verbose),
            set_global_flag(Options, force),
            set_global_flag(Options, recurse),
            set_global_var(Options, template_ext),
            set_global_var(Options, vars_ext),
            set_global_var(Options, outdir),
            set_global_var(Options, outfile),

            NonOptArgs;

        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            help(),
            halt(1)
    end.

show_info_maybe_halt(Opts) ->
    false = show_info_maybe_halt(help, Opts, fun help/0),
    false = show_info_maybe_halt(version, Opts, fun version/0).

show_info_maybe_halt(O, Opts, F) ->
    case proplists:get_bool(O, Opts) of
        true ->
            F(),
            halt(0);
        false ->
            false
    end.

set_global_flag(Options, Flag) ->
    Value = case proplists:get_bool(Flag, Options) of
                true ->
                    "1";
                false ->
                    "0"
            end,
    handlebar_config:set_global(Flag, Value).

set_global_var(Options, Var) ->
    Value = case proplists:get_value(Var, Options, undefined) of
                undefined ->
                    handlebar_config:get_global(Var,[]);
                V -> V
            end,
    handlebar_config:set_global(Var, Value).






help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "handlebar",
                 "[var=value,...] <command,...>",
                 [{"var=value", "handlebar global variabes (e.g. ext=foo)"},
                  {"files", "List of files to consume"}]).


version() ->
    {ok, Vsn} = application:get_key(handlebar, vsn),
    io:format("handlebar version: ~s~n",[Vsn]).

option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",     undefined, "Show the program options"},
     {version,  $V, "version",  undefined, "Show version information"},
     {verbose,  $v, "verbose",  undefined, "Be verbose about what gets done"},
     {force,    $f, "force",    undefined, "Force"},
     {recurse, $r, "recurse", undefined, "Recurse into directories"},
     {template_ext, $e, "template_ext", string, "Extension of the template file(s)"},
     {vars_ext, $E, "vars_ext", string, "Extension of the vars file(s)"},
     {outdir, $d, "outdir", string, "Output directory"},
     {outfile, $o, "outfile", string, "Output File, defaults to stdout"}
    ].
