-module(cucumis_gherkin_parser_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Tests ---------------------------------------------------------------------

parse_test_() ->
    {inparallel, [
        {filename:basename(F, ".feature"), fun() ->
            ?assertMatch(#{}, cucumis:parse_file(F))
        end}
     || F <- filelib:wildcard("test/files/*.feature")
    ]}.

tables_with_row_keys_test() ->
    Text = """
        Feature: tables
            Scenario: Row keys
                Given some airport locations:
                    |      |       lat |         lon |
                    | KMSY | 29.993333 |  -90.258056 |
                    | KSFO | 37.618889 | -122.375000 |
                    | KSEA | 47.448889 | -122.309444 |
                    | KJFK | 40.639722 |  -73.778889 |
    """,
    Table = [
        {~"KMSY", #{~"lat" => ~"29.993333", ~"lon" => ~"-90.258056"}},
        {~"KSFO", #{~"lat" => ~"37.618889", ~"lon" => ~"-122.375000"}},
        {~"KSEA", #{~"lat" => ~"47.448889", ~"lon" => ~"-122.309444"}},
        {~"KJFK", #{~"lat" => ~"40.639722", ~"lon" => ~"-73.778889"}}
    ],
    Step = {given, <<"some airport locations">>, [Table]},
    Scenario = #{name => <<"Row keys">>, type => scenario, steps => [Step]},
    Rule = #{scenarios => [Scenario]},
    Feature = #{name => <<"tables">>, rules => [Rule]},
    ?assertEqual(Feature, cucumis:parse(Text)).

%--- Internal ------------------------------------------------------------------
