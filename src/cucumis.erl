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
    {Name, {_Init, [_ | _], _ResultFun} = Def},
    #{defs := Defs, order := Order} = Context
) ->
    Context#{defs := Defs#{Name => Def}, order => [Name | Order]};
load_def({Name, Def}, _Context) ->
    error({invalid_definition, Name, Def}).

module_def(Module) ->
    Result =
        case lists:member({result, 1}, Module:module_info(exports)) of
            true -> fun Module:result/1;
            false -> fun identity/1
        end,
    {Module, {Module:init(), Module:steps(), Result}}.

test_feature(#{rules := Rules} = _Feature, Context) ->
    lists:foldl(fun test_rule/2, Context, Rules).

test_rule(#{scenarios := Scenarios} = _Rule, Context) ->
    lists:foldl(fun test_scenario/2, Context, Scenarios).

test_scenario(#{steps := Steps} = _Scenario, Context) ->
    lists:foldl(fun test_step/2, Context, Steps).

test_step(Step, #{order := Order} = Context) ->
    find_step(Step, Order, Context).

find_step(Step, [], #{defs := Defs}) ->
    error({no_definition, Step, maps:keys(Defs)});
find_step(Step, [Name | Rest], #{defs := Defs, env := Env} = Context) ->
    {State, NameDefs, ResultFun} = maps:get(Name, Defs),
    case find_def(Step, NameDefs) of
        {step, Fun} ->
            {Result, NewState, NewEnv} = Fun(State, Env),
            NewContext = Context#{
                status := Result,
                defs := Defs#{Name := {NewState, NameDefs, ResultFun}},
                env := NewEnv
            },
            handle_result(Result, NewContext);
        not_found ->
            find_step(Step, Rest, Context)
    end.

find_def(_Step, []) -> not_found;
find_def({_Type, Desc, _Args}, [{Desc, Fun} | _]) -> {step, Fun};
find_def(Step, [_ | Rest]) -> find_def(Step, Rest).

handle_result(success, Context) -> Context;
handle_result(failure, Context) -> throw({failure, Context}).

result(#{status := success, defs := Defs} = _Context) ->
    {success, map_results(Defs)};
result(#{status := failure, defs := Defs} = _Context) ->
    {failure, map_results(Defs)}.

map_results(Defs) -> maps:map(fun map_result/2, Defs).

map_result(_Name, {State, _Defs, ResultFun}) -> ResultFun(State).

identity(Term) -> Term.
