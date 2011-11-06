%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2011,
%%% @doc
%%%
%%% @end
%%% Created : 30 Mar 2011 by  <>
%%%-------------------------------------------------------------------
-module(handlebar_config).


-export([set_global/2,
         get_global/2,
         get_global/1
        ]).

-spec set_global(Key :: atom(), Value :: term()) -> ok.
set_global(Key,Value) ->
    application:set_env(handlebar, Key,Value).

-spec get_global(Key :: atom(), Default :: term()) -> term().
get_global(Key,Default) ->
    case application:get_env(handlebar, Key) of
        undefined ->
            Default;
        {ok,Value} ->
            Value
    end.

-spec get_global(Key :: atom()) -> term() | undefined.
get_global(Key) ->
    get_global(Key,undefined).

