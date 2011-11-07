-define(FAIL, throw({error, failed})).

-define(ABORT(Str, Args), handlebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), handlebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), handlebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), handlebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), handlebar_log:log(error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
