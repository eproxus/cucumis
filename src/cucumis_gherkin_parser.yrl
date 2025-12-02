Header "".

Nonterminals
    featurefile
    feature_heading
    section_heading
    section_start
    description
    texts
    background_scenario
    elements
    element
    element_body
    scenario_outline_heading
    examples_sections
    examples_section
    maybe_tags
    table
    table_rows
    steps
    step
    step_keyword
    step_extra
    docstring.

Terminals
    feature
    text
    background
    rule
    scenario
    scenario_outline
    tags
    examples
    example
    given
    when
    then
    and
    but
    doc_quote
    doc_tick
    table_row.

Rootsymbol
    featurefile.

featurefile -> feature_heading background_scenario elements : merge(['$1', '$2', #{rules => group_elements('$3')}]).

feature_heading -> maybe_tags feature description : merge(['$1', #{name => value('$2')}, '$3']).

description -> '$empty' : #{}.
description -> texts : #{description => string:trim(join($\s, '$1'))}.

texts -> text : [value('$1')].
texts -> text texts : [value('$1') | '$2'].

background_scenario -> '$empty' : #{}.
background_scenario -> background steps : #{background => '$2'}.

elements -> '$empty' : [].
elements -> element elements : ['$1' | '$2'].

% LALR(1) conflict resolution:
% Flattened structure allows parsing 'maybe_tags' before deciding if it's a Rule or Scenario.
element -> maybe_tags element_body : merge(['$1', '$2']).

element_body -> section_heading steps : merge(['$1', #{type => scenario, steps => '$2'}]).
element_body -> scenario_outline_heading steps examples_sections : merge(['$1', #{type => scenario, steps => '$2', examples => '$3'}]).
element_body -> rule description background_scenario : merge([#{type => rule, name => value('$1')}, '$2', '$3']).

examples_sections -> '$empty' : [].
examples_sections -> examples_section examples_sections : ['$1' | '$2'].

examples_section -> examples table : '$2'.

section_heading -> section_start description : merge([#{name => value('$1')}, '$2']).

scenario_outline_heading -> scenario_outline description : merge([#{name => value('$1')}, '$2']).

section_start -> scenario : '$1'.
section_start -> example : '$1'.

maybe_tags -> '$empty' : #{}.
maybe_tags -> tags : #{tags => value('$1')}.

steps -> '$empty' : [].
steps -> step steps : lists:flatten(['$1' | '$2']).

step -> step_keyword step_extra : {tag('$1'), value('$1'), '$2'}.

step_keyword -> given : '$1'.
step_keyword -> when : '$1'.
step_keyword -> then : '$1'.
step_keyword -> and : '$1'.
step_keyword -> but : '$1'.

step_extra -> '$empty' : [].
step_extra -> docstring : ['$1'].
step_extra -> table : ['$1'].

docstring -> doc_quote texts doc_quote : join($\n, deindent('$1', '$2')).
docstring -> doc_tick texts doc_tick : join($\n, deindent('$1', '$2')).

table -> table_rows : table('$1').

table_rows -> table_row : [value('$1')].
table_rows -> table_row table_rows : [value('$1')|'$2'].

Erlang code.
-ignore_xref(return_error/2).
-export([content/1]).
-export([file/1]).

%--- API -----------------------------------------------------------------------

content(Content) -> parse_tokens(cucumis_gherkin_scanner:content(Content)).

file(Filename) -> parse_tokens(cucumis_gherkin_scanner:file(Filename)).

%--- Internal ------------------------------------------------------------------

parse_tokens(Tokens) ->
    {ok, Features} = parse(Tokens),
    Features.

merge(Maps) ->
    lists:foldl(fun maps:merge/2, #{}, Maps).

tag(Token) -> element(1, Token).

value(Token) -> element(3, Token).

join(Sep, Texts) ->
    iolist_to_binary(lists:join(Sep, Texts)).

deindent({_Doc, _Loc, Chars}, Lines) ->
    {match, [Indent]} = re:run(Chars, "\s+", [{capture, first, list}]),
    [string:prefix(L, Indent) || L <- Lines].

table([Header | Rows]) ->
    Fun =
        case split_row(Header) of
            [<<>> | HeaderCells] ->
                fun(R) ->
                    [RK | RC] = split_row(R),
                    {RK, row(HeaderCells, RC)}
                end;
            HeaderCells ->
                fun(R) -> row(HeaderCells, split_row(R)) end
        end,
    lists:map(Fun, Rows).

row(Header, RowCells) ->
    #{K => string:trim(V) || K <- Header && V <- RowCells}.

split_row(Row) ->
    [<<>> | Cells] = re:split(Row, ~B"\s*(?<!\\)\|\s*", [trim]),
    Cells.

group_elements(Elements) ->
    {ImplicitScenarios, Rules} = lists:foldr(
        fun
            (#{type := rule} = Rule, {Scenarios, RulesAcc}) ->
                {[], [Rule#{scenarios => Scenarios} | RulesAcc]};
            (Scenario, {Scenarios, RulesAcc}) ->
                {[Scenario | Scenarios], RulesAcc}
        end,
        {[], []},
        Elements
    ),

    case ImplicitScenarios of
        [] -> Rules;
        _ -> [#{scenarios => ImplicitScenarios} | Rules]
    end.
