-module(cucumis_gherkin_parser_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Tests ---------------------------------------------------------------------

parse_test_() ->
    {inparallel, [
        {filename:basename(F, ".feature"), fun() ->
            ?assertMatch(#{}, cucumis_gherkin_parser:file(F))
        end}
     || F <- filelib:wildcard("test/files/*.feature")
    ]}.

%--- Internal ------------------------------------------------------------------
