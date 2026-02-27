-module(cucumis).

-behaviour(application).

% API
-export([parse/1]).
-export([parse_file/1]).
-export([test/2]).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- API -----------------------------------------------------------------------

parse(Content) -> cucumis_gherkin_parser:content(Content).

parse_file(File) -> cucumis_gherkin_parser:file(File).

test(Feature, Opts) ->
    NewContext =
        try
            Context = #{
                status => success, defs => #{}, order => [], env => #{}
            },
            test_feature(load_feature(Feature), load_defs(Opts, Context))
        catch
            {failure, C} ->
                C;
            {no_definition, Step} ->
                error({no_definition, Step, maps:get(definitions, Opts)})
        end,
    result(NewContext).

%--- Callbacks -----------------------------------------------------------------

start(_StartType, _StartArgs) -> cucumis_sup:start_link().

stop(_State) -> ok.

%--- Internal ------------------------------------------------------------------

load_feature(Feature) when is_map(Feature) ->
    Feature;
load_feature(Feature) when is_list(Feature); is_binary(Feature) ->
    parse_file(Feature).

load_defs(#{definitions := Defs}, Context) ->
    NewContext = #{order := Order} = lists:foldl(fun load_def/2, Context, Defs),
    NewContext#{order := lists:reverse(Order)}.

load_def(Def, Context) when is_atom(Def) ->
    load_def(module_def(Def), Context);
load_def(
    {Name, {Init, DefSteps, ResultFun}},
    #{defs := Defs, order := Order} = Context
) ->
    Definition = {Init, compile_steps(DefSteps), ResultFun},
    Context#{
        defs := Defs#{Name => Definition},
        order => [Name | Order]
    };
load_def({Name, Def}, _Context) ->
    error({invalid_definition, Name, Def}).

compile_steps(Steps) ->
    lists:map(fun compile_step/1, Steps).

compile_step({Regex, Fun}) ->
    RegexWordMatch = re:replace(
        Regex,
        % Match $word only if preceded by an even number of backslashes,
        % including zero (e.g `$user`, `$email` etc.)
        ~B"(?<!\\)(?:\\\\)*\$([[:word:]]+)",
        % Replace with a named group (e.g. `(?<user>[[:word:]]+)`)
        ~B"(?<\1>[[:word:]]+)",
        % Replace all occurences
        [global]
    ),
    {ok, Re} = re:compile(RegexWordMatch, [anchored]),
    {namelist, Names} = re:inspect(Re, namelist),
    {{Re, Names}, Fun}.

module_def(Module) ->
    Result =
        case lists:member({result, 1}, Module:module_info(exports)) of
            true -> fun Module:result/1;
            false -> fun identity/1
        end,
    {Module, {Module:init(), Module:steps(), Result}}.

test_feature(#{rules := Rules} = _Feature, Context) ->
    lists:foldl(fun test_rule/2, Context, Rules).

test_background([], Context) ->
    Context;
test_background(Steps, Context) ->
    lists:foldl(fun test_step/2, Context, Steps).

test_rule(#{scenarios := Scenarios} = Rule, Context) ->
    Background = maps:get(background, Rule, []),
    lists:foldl(
        fun(S, C) -> test_scenario(S, Background, C) end,
        Context,
        Scenarios
    ).

test_scenario(#{steps := Steps} = _Scenario, Background, Context) ->
    lists:foldl(fun test_step/2, test_background(Background, Context), Steps).

test_step(Step, #{order := Order} = Context) ->
    find_step(Step, Order, Context).

find_step(Step, [], #{defs := Defs}) ->
    error({no_definition, Step, maps:keys(Defs)});
find_step(Step, [Name | Rest], #{defs := Defs, env := Env} = Context) ->
    {State, NameDefs, ResultFun} = maps:get(Name, Defs),
    case find_def(Step, NameDefs) of
        {step, Fun, Matches, Args} ->
            {Result, NewState, NewEnv} =
                case erlang:fun_info(Fun, arity) of
                    {arity, 3} -> Fun(Matches, State, Env);
                    {arity, 4} -> Fun(Matches, State, Env, Args)
                end,
            NewContext = Context#{
                status := Result,
                defs := Defs#{Name := {NewState, NameDefs, ResultFun}},
                env := NewEnv
            },
            handle_result(Result, NewContext);
        not_found ->
            find_step(Step, Rest, Context)
    end.

find_def(_Step, []) ->
    not_found;
find_def({_Type, StepText, Args} = Step, [{{Regex, Names}, Fun} | Rest]) ->
    case re:run(StepText, Regex, [{capture, all_but_first, binary}]) of
        {match, Result} ->
            Named =
                case re:run(StepText, Regex, [{capture, all_names, binary}]) of
                    {match, NameMatches} ->
                        #{
                            binary_to_atom(K) => V
                         || {K, V} <- lists:zip(Names, NameMatches)
                        };
                    _ ->
                        #{}
                end,
            Matches = maps:merge(
                maps:from_list(lists:enumerate(Result)),
                Named
            ),
            {step, Fun, Matches, Args};
        nomatch ->
            find_def(Step, Rest)
    end.

handle_result(success, Context) -> Context;
handle_result(failure, Context) -> throw({failure, Context}).

result(#{status := success, defs := Defs} = _Context) ->
    {success, map_results(Defs)};
result(#{status := failure, defs := Defs} = _Context) ->
    {failure, map_results(Defs)}.

map_results(Defs) ->
    maps:map(fun map_result/2, Defs).

map_result(_Name, {State, _Defs, ResultFun}) -> ResultFun(State).

identity(Term) -> Term.
