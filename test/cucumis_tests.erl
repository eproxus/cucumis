-module(cucumis_tests).

-export([init/0]).
-export([steps/0]).
-export([result/1]).

-include_lib("eunit/include/eunit.hrl").

%--- Tests ---------------------------------------------------------------------

app_test() ->
    without_error_logger(fun() ->
        Start = application:ensure_all_started(cucumis),
        ?assertMatch({ok, _}, Start),
        {ok, Apps} = Start,
        [?assertEqual(ok, application:stop(A)) || A <- Apps]
    end).

feature_file_does_not_exist_test() ->
    ?assertError(
        {file_not_found, "foobar"},
        cucumis:test("foobar", #{})
    ).

feature_no_definition_test() ->
    Feature = #{
        rules => [#{scenarios => [#{steps => [{given, ~"foobar", []}]}]}]
    },
    ?assertError(
        {no_definition, {given, ~"foobar", []}, [?MODULE]},
        cucumis:test(Feature, #{definitions => [?MODULE]})
    ).

feature_invalid_definition_test() ->
    Feature = #{
        rules => [#{scenarios => [#{steps => [{given, ~"foobar", []}]}]}]
    },
    ?assertError(
        {invalid_definition, foo, []},
        cucumis:test(Feature, #{definitions => [{foo, []}]})
    ).

feature_fail_test() ->
    Feature = #{
        rules => [#{scenarios => [#{steps => [{given, ~"foobar", []}]}]}]
    },
    Opts = #{
        definitions => [
            {foo,
                {
                    s,
                    [
                        {~"foobar", fun(_Match, State, Env) ->
                            {failure, State, Env}
                        end}
                    ],
                    fun(S) -> {r, S} end
                }}
        ]
    },
    ?assertEqual({failure, #{foo => {r, s}}}, cucumis:test(Feature, Opts)).

feature_module_test() ->
    Result = cucumis:test("test/files/guess_the_word.feature", #{
        definitions => [?MODULE]
    }),
    ?assertEqual(
        {success, #{
            ?MODULE => [
                started,
                waiting,
                {word, ~"Maker", ~"silky"},
                joined,
                guess
            ]
        }},
        Result
    ).

%--- Definitions ---------------------------------------------------------------

init() -> [].

steps() ->
    [
        {
            ~"the Maker starts a game",
            fun(_Match, State, Env) -> {success, [started | State], Env} end
        },
        {
            ~"the Maker waits for a Breaker to join",
            fun(_Match, State, Env) -> {success, [waiting | State], Env} end
        },
        {
            ~"the $player has started a game with the word \"(?<word>[[:word:]]+)\"",
            fun(
                % It is possible to refer to matches by keyword or index
                #{player := Player, 1 := Player, word := Word, 2 := Word},
                State,
                Env
            ) ->
                {success, [{word, Player, Word} | State], Env}
            end
        },
        {
            ~"the Breaker joins the Maker's game",
            fun(_Match, State, Env) -> {success, [joined | State], Env} end
        },
        {
            ~"the Breaker must guess a word with 5 characters",
            fun(_Match, State, Env) -> {success, [guess | State], Env} end
        }
    ].

result(S) -> lists:reverse(S).

%--- Internal ------------------------------------------------------------------

without_error_logger(Fun) ->
    error_logger:tty(false),
    try
        Fun()
    after
        error_logger:tty(true)
    end.
