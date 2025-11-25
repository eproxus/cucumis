-module(cucumis_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Tests ---------------------------------------------------------------------

app_test() ->
    without_error_logger(fun() ->
        Start = application:ensure_all_started(cucumis),
        ?assertMatch({ok, _}, Start),
        {ok, Apps} = Start,
        [?assertEqual(ok, application:stop(A)) || A <- Apps]
    end).

%--- Internal ------------------------------------------------------------------

without_error_logger(Fun) ->
    error_logger:tty(false),
    try
        Fun()
    after
        error_logger:tty(true)
    end.
