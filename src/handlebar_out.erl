-module(handlebar_out).



-export([
         output/2
         ]).

-include("handlebar.hrl").


output(Template, Data) ->
    ?DEBUG("HANDLING ~p->~p~n",[Template,Data]),

    %% we can do one of three things
    %% 1. stdout
    %% 2. filename (append or overwrite?)
    %% 3. dir/filename

    %% is outdir specified?
    case handlebar_config:get_global(outdir,undefined) of
        undefined ->
            %% is outfile specified?
            case handlebar_config:get_global(outfile, undefined) of
                undefined ->
                    %% nothing specified, dump straight to stdout
                    io:format(Data);
                File ->
                    output_file(File, Data)
            end;
        Dir ->
            output_dir(Dir, Template, Data)
    end.

output_file(File, Data) ->
    case filelib:ensure_dir(File) of
        {error, Reason} ->
            ?ERROR("Can not ensure dir for file ~p:~p~n",[File, Reason]);
        ok ->
            case file:write_file(File, Data) of
                {error, Reason2} ->
                    ?ERROR("write error for ~p~n~p~n",[File, Reason2]);
                ok -> ok
            end
    end.


output_dir(Dir, Template, Data) ->

    FName = filename:rootname(filename:basename(Template)),
    FullPath = filename:join([Dir,FName]),
    ?DEBUG("OUTPUT:~p~n",[FullPath]),
    output_file(FullPath, Data).
