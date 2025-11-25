-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 0).
-module(cucumis_gherkin_parser).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.erl", 4).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file(
    "/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 110
).
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

-file(
    "/Users/alind/.local/share/mise/installs/erlang/28.2/lib/parsetools-2.7/include/yeccpre.hrl",
    0
).
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-ifdef(YECC_PARSE_DOC).
-doc ?YECC_PARSE_DOC.
-endif.
-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_location}, 0, [], []).

-ifdef(YECC_PARSE_AND_SCAN_DOC).
-doc ?YECC_PARSE_AND_SCAN_DOC.
-endif.
-spec parse_and_scan(
    {function() | {atom(), atom()}, [_]}
    | {atom(), atom(), [_]}
) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_location}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_location}, 0, [], []).

-ifdef(YECC_FORMAT_ERROR_DOC).
-doc ?YECC_FORMAT_ERROR_DOC.
-endif.
-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(erl_anno:location(), any()) -> no_return().
return_error(Location, Message) ->
    throw({error, {Location, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try
        yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch
        error:Error:Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(
                        error,
                        {yecc_bug, ?CODE_VERSION, Desc},
                        Stacktrace
                    )
            catch
                _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw:{error, {_Location, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE, F, ArityOrArgs, _} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok, [{atom, _, Symbol}], _} = erl_scan:string(SymbolL),
            State =
                case ArityOrArgs of
                    [S, _, _, _, _, _, _] -> S;
                    _ -> state_is_unknown
                end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A}, _Location}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, EndLocation} ->
            yeccpars1(Tokens, {{F, A}, EndLocation}, State, States, Vstack);
        {eof, EndLocation} ->
            yeccpars1([], {no_func, EndLocation}, State, States, Vstack);
        {error, Descriptor, _EndLocation} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_location}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(
        State,
        '$end',
        States,
        Vstack,
        yecc_end(Line),
        [],
        {no_func, Line}
    );
yeccpars1([], {no_func, EndLocation}, State, States, Vstack) ->
    yeccpars2(
        State,
        '$end',
        States,
        Vstack,
        yecc_end(EndLocation),
        [],
        {no_func, EndLocation}
    ).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(
        State,
        element(1, Token),
        [State1 | States],
        [Token0 | Vstack],
        Token,
        Tokens,
        Tzr
    );
yeccpars1(
    State1, State, States, Vstack, Token0, [], {{_F, _A}, _Location} = Tzr
) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_location}) ->
    Location = yecctoken_end_location(Token0),
    yeccpars2(
        State,
        '$end',
        [State1 | States],
        [Token0 | Vstack],
        yecc_end(Location),
        [],
        {no_func, Location}
    );
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Location}) ->
    yeccpars2(
        State,
        '$end',
        [State1 | States],
        [Token0 | Vstack],
        yecc_end(Location),
        [],
        {no_func, Location}
    ).

%% For internal use only.
yecc_end(Location) ->
    {'$end', Location}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch
        _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch
        _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try
        erl_scan:location(Token)
    catch
        _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string(Token) ->
    try
        yecctoken2string1(Token)
    catch
        _:_ ->
            io_lib:format("~tp", [Token])
    end.

-compile({nowarn_unused_function, yecctoken2string1/1}).
yecctoken2string1({atom, _, A}) ->
    io_lib:write_atom(A);
yecctoken2string1({integer, _, N}) ->
    io_lib:write(N);
yecctoken2string1({float, _, F}) ->
    io_lib:write(F);
yecctoken2string1({char, _, C}) ->
    io_lib:write_char(C);
yecctoken2string1({var, _, V}) ->
    io_lib:format("~s", [V]);
yecctoken2string1({string, _, S}) ->
    io_lib:write_string(S);
yecctoken2string1({reserved_symbol, _, A}) ->
    io_lib:write(A);
yecctoken2string1({_Cat, _, Val}) ->
    io_lib:format("~tp", [Val]);
yecctoken2string1({dot, _}) ->
    "'.'";
yecctoken2string1({'$end', _}) ->
    [];
yecctoken2string1({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string1(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-file(
    "/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.erl", 264
).

-dialyzer({nowarn_function, yeccpars2/7}).
-compile({nowarn_unused_function, yeccpars2/7}).
yeccpars2(0 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
    erlang:error({yecc_bug, "1.4", {missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
-compile({nowarn_unused_function, yeccpars2_0/7}).
yeccpars2_0(S, 'tags', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_0_(Stack),
    yeccpars2_1(1, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_1/7}).
-compile({nowarn_unused_function, yeccpars2_1/7}).
yeccpars2_1(S, 'feature', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_2/7}).
-compile({nowarn_unused_function, yeccpars2_2/7}).
yeccpars2_2(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
    {ok, hd(Stack)};
yeccpars2_2(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_3/7}).
-compile({nowarn_unused_function, yeccpars2_3/7}).
yeccpars2_3(S, 'background', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_3_(Stack),
    yeccpars2_5(5, Cat, [3 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_4/7}).
-compile({nowarn_unused_function, yeccpars2_4/7}).
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_4_(Stack),
    yeccgoto_maybe_tags(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_5/7}).
-compile({nowarn_unused_function, yeccpars2_5/7}).
yeccpars2_5(S, 'tags', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
    NewStack = 'yeccpars2_5_$end'(Stack),
    yeccpars2_31(_S, '$end', [5 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_5_(Stack),
    yeccpars2_30(30, Cat, [5 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).
-compile({nowarn_unused_function, yeccpars2_6/7}).
yeccpars2_6(S, 'and', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, 'but', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, 'given', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, 'then', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, 'when', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_6_(Stack),
    yeccpars2_7(_S, Cat, [6 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_7/7}).
-compile({nowarn_unused_function, yeccpars2_7/7}).
yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_7_(Stack),
    yeccgoto_background_scenario(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
-compile({nowarn_unused_function, yeccpars2_8/7}).
yeccpars2_8(S, 'doc_quote', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, 'doc_tick', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, 'table_row', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_8_(Stack),
    yeccpars2_18(_S, Cat, [8 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_9/7}).
-compile({nowarn_unused_function, yeccpars2_9/7}).
yeccpars2_9(S, 'and', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, 'but', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, 'given', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, 'then', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, 'when', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_9_(Stack),
    yeccpars2_15(_S, Cat, [9 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_10/7}).
-compile({nowarn_unused_function, yeccpars2_10/7}).
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_10_(Stack),
    yeccgoto_step_keyword(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_11/7}).
-compile({nowarn_unused_function, yeccpars2_11/7}).
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_11_(Stack),
    yeccgoto_step_keyword(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_12/7}).
-compile({nowarn_unused_function, yeccpars2_12/7}).
yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_12_(Stack),
    yeccgoto_step_keyword(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_13/7}).
-compile({nowarn_unused_function, yeccpars2_13/7}).
yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_13_(Stack),
    yeccgoto_step_keyword(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_14/7}).
-compile({nowarn_unused_function, yeccpars2_14/7}).
yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_14_(Stack),
    yeccgoto_step_keyword(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_15/7}).
-compile({nowarn_unused_function, yeccpars2_15/7}).
yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_15_(Stack),
    yeccgoto_steps(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_16/7}).
-compile({nowarn_unused_function, yeccpars2_16/7}).
yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_16_(Stack),
    yeccgoto_table(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_17/7}).
-compile({nowarn_unused_function, yeccpars2_17/7}).
yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_17_(Stack),
    yeccgoto_step_extra(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_18/7}).
-compile({nowarn_unused_function, yeccpars2_18/7}).
yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_18_(Stack),
    yeccgoto_step(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_19/7}).
-compile({nowarn_unused_function, yeccpars2_19/7}).
yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_19_(Stack),
    yeccgoto_step_extra(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_20/7}).
-compile({nowarn_unused_function, yeccpars2_20/7}).
yeccpars2_20(S, 'text', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_21: see yeccpars2_20

-dialyzer({nowarn_function, yeccpars2_22/7}).
-compile({nowarn_unused_function, yeccpars2_22/7}).
yeccpars2_22(S, 'table_row', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_22_(Stack),
    yeccgoto_table_rows(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_23/7}).
-compile({nowarn_unused_function, yeccpars2_23/7}).
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_23_(Stack),
    yeccgoto_table_rows(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_24/7}).
-compile({nowarn_unused_function, yeccpars2_24/7}).
yeccpars2_24(S, 'doc_tick', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_25/7}).
-compile({nowarn_unused_function, yeccpars2_25/7}).
yeccpars2_25(S, 'text', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_25_(Stack),
    yeccgoto_texts(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_26/7}).
-compile({nowarn_unused_function, yeccpars2_26/7}).
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_26_(Stack),
    yeccgoto_texts(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_27/7}).
-compile({nowarn_unused_function, yeccpars2_27/7}).
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_27_(Stack),
    yeccgoto_docstring(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_28/7}).
-compile({nowarn_unused_function, yeccpars2_28/7}).
yeccpars2_28(S, 'doc_quote', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_29/7}).
-compile({nowarn_unused_function, yeccpars2_29/7}).
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_29_(Stack),
    yeccgoto_docstring(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_30/7}).
-compile({nowarn_unused_function, yeccpars2_30/7}).
yeccpars2_30(S, 'example', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 'rule', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 'scenario', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 'scenario_outline', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_31/7}).
-compile({nowarn_unused_function, yeccpars2_31/7}).
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_31_(Stack),
    yeccgoto_featurefile(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
-compile({nowarn_unused_function, yeccpars2_32/7}).
yeccpars2_32(S, 'tags', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
    NewStack = 'yeccpars2_32_$end'(Stack),
    yeccpars2_33(_S, '$end', [32 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_32_(Stack),
    yeccpars2_30(30, Cat, [32 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_33/7}).
-compile({nowarn_unused_function, yeccpars2_33/7}).
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_33_(Stack),
    yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_34/7}).
-compile({nowarn_unused_function, yeccpars2_34/7}).
yeccpars2_34(S, 'text', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_34_(Stack),
    yeccpars2_53(_S, Cat, [34 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).
-compile({nowarn_unused_function, yeccpars2_35/7}).
yeccpars2_35(S, 'and', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, 'but', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, 'given', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, 'then', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, 'when', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_35_(Stack),
    yeccpars2_52(_S, Cat, [35 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_36/7}).
-compile({nowarn_unused_function, yeccpars2_36/7}).
yeccpars2_36(S, 'and', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, 'but', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, 'given', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, 'then', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, 'when', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_36_(Stack),
    yeccpars2_46(46, Cat, [36 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_37/7}).
-compile({nowarn_unused_function, yeccpars2_37/7}).
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_37_(Stack),
    yeccgoto_element(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_38/7}).
-compile({nowarn_unused_function, yeccpars2_38/7}).
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_38_(Stack),
    yeccgoto_section_start(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_39/7}).
-compile({nowarn_unused_function, yeccpars2_39/7}).
yeccpars2_39(S, 'text', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_39_(Stack),
    yeccpars2_44(44, Cat, [39 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_40/7}).
-compile({nowarn_unused_function, yeccpars2_40/7}).
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_40_(Stack),
    yeccgoto_section_start(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_41/7}).
-compile({nowarn_unused_function, yeccpars2_41/7}).
yeccpars2_41(S, 'text', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_41_(Stack),
    yeccpars2_43(_S, Cat, [41 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_42/7}).
-compile({nowarn_unused_function, yeccpars2_42/7}).
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_42_(Stack),
    yeccgoto_description(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_43/7}).
-compile({nowarn_unused_function, yeccpars2_43/7}).
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_43_(Stack),
    yeccgoto_scenario_outline_heading(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_44/7}).
-compile({nowarn_unused_function, yeccpars2_44/7}).
yeccpars2_44(S, 'background', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_44_(Stack),
    yeccpars2_45(_S, Cat, [44 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_45/7}).
-compile({nowarn_unused_function, yeccpars2_45/7}).
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_45_(Stack),
    yeccgoto_element_body(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_46/7}).
-compile({nowarn_unused_function, yeccpars2_46/7}).
yeccpars2_46(S, 'examples', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_46_(Stack),
    yeccpars2_47(_S, Cat, [46 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_47/7}).
-compile({nowarn_unused_function, yeccpars2_47/7}).
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_47_(Stack),
    yeccgoto_element_body(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_48/7}).
-compile({nowarn_unused_function, yeccpars2_48/7}).
yeccpars2_48(S, 'examples', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_48_(Stack),
    yeccpars2_51(_S, Cat, [48 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_49/7}).
-compile({nowarn_unused_function, yeccpars2_49/7}).
yeccpars2_49(S, 'table_row', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_50/7}).
-compile({nowarn_unused_function, yeccpars2_50/7}).
yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_50_(Stack),
    yeccgoto_examples_section(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_51/7}).
-compile({nowarn_unused_function, yeccpars2_51/7}).
yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_51_(Stack),
    yeccgoto_examples_sections(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_52/7}).
-compile({nowarn_unused_function, yeccpars2_52/7}).
yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_52_(Stack),
    yeccgoto_element_body(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_53/7}).
-compile({nowarn_unused_function, yeccpars2_53/7}).
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_53_(Stack),
    yeccgoto_section_heading(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_54/7}).
-compile({nowarn_unused_function, yeccpars2_54/7}).
yeccpars2_54(S, 'text', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_54_(Stack),
    yeccpars2_55(_S, Cat, [54 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_55/7}).
-compile({nowarn_unused_function, yeccpars2_55/7}).
yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_55_(Stack),
    yeccgoto_feature_heading(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_background_scenario/7}).
-compile({nowarn_unused_function, yeccgoto_background_scenario/7}).
yeccgoto_background_scenario(3, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_background_scenario(44 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_description/7}).
-compile({nowarn_unused_function, yeccgoto_description/7}).
yeccgoto_description(34 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_description(39, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_description(41 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_description(54 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_docstring/7}).
-compile({nowarn_unused_function, yeccgoto_docstring/7}).
yeccgoto_docstring(8 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_element/7}).
-compile({nowarn_unused_function, yeccgoto_element/7}).
yeccgoto_element(5, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(32, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_element_body/7}).
-compile({nowarn_unused_function, yeccgoto_element_body/7}).
yeccgoto_element_body(30 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_elements/7}).
-compile({nowarn_unused_function, yeccgoto_elements/7}).
yeccgoto_elements(5 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(32 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_examples_section/7}).
-compile({nowarn_unused_function, yeccgoto_examples_section/7}).
yeccgoto_examples_section(46, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_examples_section(48, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_examples_sections/7}).
-compile({nowarn_unused_function, yeccgoto_examples_sections/7}).
yeccgoto_examples_sections(46 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_examples_sections(48 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_feature_heading/7}).
-compile({nowarn_unused_function, yeccgoto_feature_heading/7}).
yeccgoto_feature_heading(0, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_featurefile/7}).
-compile({nowarn_unused_function, yeccgoto_featurefile/7}).
yeccgoto_featurefile(0, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_maybe_tags/7}).
-compile({nowarn_unused_function, yeccgoto_maybe_tags/7}).
yeccgoto_maybe_tags(0, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_maybe_tags(5, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_maybe_tags(32, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_scenario_outline_heading/7}).
-compile({nowarn_unused_function, yeccgoto_scenario_outline_heading/7}).
yeccgoto_scenario_outline_heading(30, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_section_heading/7}).
-compile({nowarn_unused_function, yeccgoto_section_heading/7}).
yeccgoto_section_heading(30, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_section_start/7}).
-compile({nowarn_unused_function, yeccgoto_section_start/7}).
yeccgoto_section_start(30, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_step/7}).
-compile({nowarn_unused_function, yeccgoto_step/7}).
yeccgoto_step(6, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_step(9, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_step(35, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_step(36, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_step_extra/7}).
-compile({nowarn_unused_function, yeccgoto_step_extra/7}).
yeccgoto_step_extra(8 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_step_keyword/7}).
-compile({nowarn_unused_function, yeccgoto_step_keyword/7}).
yeccgoto_step_keyword(6, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_step_keyword(9, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_step_keyword(35, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_step_keyword(36, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_steps/7}).
-compile({nowarn_unused_function, yeccgoto_steps/7}).
yeccgoto_steps(6 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_steps(9 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_steps(35 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_steps(36, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_table/7}).
-compile({nowarn_unused_function, yeccgoto_table/7}).
yeccgoto_table(8 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_table(49 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_table_rows/7}).
-compile({nowarn_unused_function, yeccgoto_table_rows/7}).
yeccgoto_table_rows(8 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_table_rows(22 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_table_rows(49 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_texts/7}).
-compile({nowarn_unused_function, yeccgoto_texts/7}).
yeccgoto_texts(20, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_texts(21, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_24(24, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_texts(25 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_texts(34 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_texts(39 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_texts(41 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_texts(54 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline, yeccpars2_0_/1}).
-dialyzer({nowarn_function, yeccpars2_0_/1}).
-compile({nowarn_unused_function, yeccpars2_0_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 80).
yeccpars2_0_(__Stack0) ->
    [
        begin
            #{}
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_3_/1}).
-dialyzer({nowarn_function, yeccpars2_3_/1}).
-compile({nowarn_unused_function, yeccpars2_3_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 54).
yeccpars2_3_(__Stack0) ->
    [
        begin
            #{}
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_4_/1}).
-dialyzer({nowarn_function, yeccpars2_4_/1}).
-compile({nowarn_unused_function, yeccpars2_4_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 81).
yeccpars2_4_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            #{tags => value(___1)}
        end
        | __Stack
    ].

-compile({inline, 'yeccpars2_5_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_5_$end'/1}).
-compile({nowarn_unused_function, 'yeccpars2_5_$end'/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 57).
'yeccpars2_5_$end'(__Stack0) ->
    [
        begin
            []
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_5_/1}).
-dialyzer({nowarn_function, yeccpars2_5_/1}).
-compile({nowarn_unused_function, yeccpars2_5_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 80).
yeccpars2_5_(__Stack0) ->
    [
        begin
            #{}
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_6_/1}).
-dialyzer({nowarn_function, yeccpars2_6_/1}).
-compile({nowarn_unused_function, yeccpars2_6_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 83).
yeccpars2_6_(__Stack0) ->
    [
        begin
            []
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_7_/1}).
-dialyzer({nowarn_function, yeccpars2_7_/1}).
-compile({nowarn_unused_function, yeccpars2_7_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 55).
yeccpars2_7_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            #{background => ___2}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_8_/1}).
-dialyzer({nowarn_function, yeccpars2_8_/1}).
-compile({nowarn_unused_function, yeccpars2_8_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 94).
yeccpars2_8_(__Stack0) ->
    [
        begin
            []
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_9_/1}).
-dialyzer({nowarn_function, yeccpars2_9_/1}).
-compile({nowarn_unused_function, yeccpars2_9_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 83).
yeccpars2_9_(__Stack0) ->
    [
        begin
            []
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_10_/1}).
-dialyzer({nowarn_function, yeccpars2_10_/1}).
-compile({nowarn_unused_function, yeccpars2_10_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 91).
yeccpars2_10_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_11_/1}).
-dialyzer({nowarn_function, yeccpars2_11_/1}).
-compile({nowarn_unused_function, yeccpars2_11_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 92).
yeccpars2_11_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_12_/1}).
-dialyzer({nowarn_function, yeccpars2_12_/1}).
-compile({nowarn_unused_function, yeccpars2_12_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 88).
yeccpars2_12_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_13_/1}).
-dialyzer({nowarn_function, yeccpars2_13_/1}).
-compile({nowarn_unused_function, yeccpars2_13_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 90).
yeccpars2_13_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_14_/1}).
-dialyzer({nowarn_function, yeccpars2_14_/1}).
-compile({nowarn_unused_function, yeccpars2_14_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 89).
yeccpars2_14_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_15_/1}).
-dialyzer({nowarn_function, yeccpars2_15_/1}).
-compile({nowarn_unused_function, yeccpars2_15_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 84).
yeccpars2_15_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            lists:flatten([___1 | ___2])
        end
        | __Stack
    ].

-compile({inline, yeccpars2_16_/1}).
-dialyzer({nowarn_function, yeccpars2_16_/1}).
-compile({nowarn_unused_function, yeccpars2_16_/1}).
-file(
    "/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 101
).
yeccpars2_16_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            table(___1)
        end
        | __Stack
    ].

-compile({inline, yeccpars2_17_/1}).
-dialyzer({nowarn_function, yeccpars2_17_/1}).
-compile({nowarn_unused_function, yeccpars2_17_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 96).
yeccpars2_17_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            [___1]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_18_/1}).
-dialyzer({nowarn_function, yeccpars2_18_/1}).
-compile({nowarn_unused_function, yeccpars2_18_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 86).
yeccpars2_18_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {tag(___1), value(___1), ___2}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_19_/1}).
-dialyzer({nowarn_function, yeccpars2_19_/1}).
-compile({nowarn_unused_function, yeccpars2_19_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 95).
yeccpars2_19_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            [___1]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_22_/1}).
-dialyzer({nowarn_function, yeccpars2_22_/1}).
-compile({nowarn_unused_function, yeccpars2_22_/1}).
-file(
    "/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 103
).
yeccpars2_22_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            [value(___1)]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_23_/1}).
-dialyzer({nowarn_function, yeccpars2_23_/1}).
-compile({nowarn_unused_function, yeccpars2_23_/1}).
-file(
    "/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 104
).
yeccpars2_23_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            [value(___1) | ___2]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_25_/1}).
-dialyzer({nowarn_function, yeccpars2_25_/1}).
-compile({nowarn_unused_function, yeccpars2_25_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 51).
yeccpars2_25_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            [value(___1)]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_26_/1}).
-dialyzer({nowarn_function, yeccpars2_26_/1}).
-compile({nowarn_unused_function, yeccpars2_26_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 52).
yeccpars2_26_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            [value(___1) | ___2]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_27_/1}).
-dialyzer({nowarn_function, yeccpars2_27_/1}).
-compile({nowarn_unused_function, yeccpars2_27_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 99).
yeccpars2_27_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            join($\n, deindent(___1, ___2))
        end
        | __Stack
    ].

-compile({inline, yeccpars2_29_/1}).
-dialyzer({nowarn_function, yeccpars2_29_/1}).
-compile({nowarn_unused_function, yeccpars2_29_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 98).
yeccpars2_29_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            join($\n, deindent(___1, ___2))
        end
        | __Stack
    ].

-compile({inline, yeccpars2_31_/1}).
-dialyzer({nowarn_function, yeccpars2_31_/1}).
-compile({nowarn_unused_function, yeccpars2_31_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 44).
yeccpars2_31_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            merge([___1, ___2, #{rules => group_elements(___3)}])
        end
        | __Stack
    ].

-compile({inline, 'yeccpars2_32_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_32_$end'/1}).
-compile({nowarn_unused_function, 'yeccpars2_32_$end'/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 57).
'yeccpars2_32_$end'(__Stack0) ->
    [
        begin
            []
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_32_/1}).
-dialyzer({nowarn_function, yeccpars2_32_/1}).
-compile({nowarn_unused_function, yeccpars2_32_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 80).
yeccpars2_32_(__Stack0) ->
    [
        begin
            #{}
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_33_/1}).
-dialyzer({nowarn_function, yeccpars2_33_/1}).
-compile({nowarn_unused_function, yeccpars2_33_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 58).
yeccpars2_33_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            [___1 | ___2]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_34_/1}).
-dialyzer({nowarn_function, yeccpars2_34_/1}).
-compile({nowarn_unused_function, yeccpars2_34_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 48).
yeccpars2_34_(__Stack0) ->
    [
        begin
            #{}
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_35_/1}).
-dialyzer({nowarn_function, yeccpars2_35_/1}).
-compile({nowarn_unused_function, yeccpars2_35_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 83).
yeccpars2_35_(__Stack0) ->
    [
        begin
            []
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_36_/1}).
-dialyzer({nowarn_function, yeccpars2_36_/1}).
-compile({nowarn_unused_function, yeccpars2_36_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 83).
yeccpars2_36_(__Stack0) ->
    [
        begin
            []
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_37_/1}).
-dialyzer({nowarn_function, yeccpars2_37_/1}).
-compile({nowarn_unused_function, yeccpars2_37_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 62).
yeccpars2_37_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            merge([___1, ___2])
        end
        | __Stack
    ].

-compile({inline, yeccpars2_38_/1}).
-dialyzer({nowarn_function, yeccpars2_38_/1}).
-compile({nowarn_unused_function, yeccpars2_38_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 78).
yeccpars2_38_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_39_/1}).
-dialyzer({nowarn_function, yeccpars2_39_/1}).
-compile({nowarn_unused_function, yeccpars2_39_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 48).
yeccpars2_39_(__Stack0) ->
    [
        begin
            #{}
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_40_/1}).
-dialyzer({nowarn_function, yeccpars2_40_/1}).
-compile({nowarn_unused_function, yeccpars2_40_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 77).
yeccpars2_40_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_41_/1}).
-dialyzer({nowarn_function, yeccpars2_41_/1}).
-compile({nowarn_unused_function, yeccpars2_41_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 48).
yeccpars2_41_(__Stack0) ->
    [
        begin
            #{}
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_42_/1}).
-dialyzer({nowarn_function, yeccpars2_42_/1}).
-compile({nowarn_unused_function, yeccpars2_42_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 49).
yeccpars2_42_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            #{description => string:trim(join($\s, ___1))}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_43_/1}).
-dialyzer({nowarn_function, yeccpars2_43_/1}).
-compile({nowarn_unused_function, yeccpars2_43_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 75).
yeccpars2_43_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            merge([#{name => value(___1)}, ___2])
        end
        | __Stack
    ].

-compile({inline, yeccpars2_44_/1}).
-dialyzer({nowarn_function, yeccpars2_44_/1}).
-compile({nowarn_unused_function, yeccpars2_44_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 54).
yeccpars2_44_(__Stack0) ->
    [
        begin
            #{}
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_45_/1}).
-dialyzer({nowarn_function, yeccpars2_45_/1}).
-compile({nowarn_unused_function, yeccpars2_45_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 66).
yeccpars2_45_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            merge([#{type => rule, name => value(___1)}, ___2, ___3])
        end
        | __Stack
    ].

-compile({inline, yeccpars2_46_/1}).
-dialyzer({nowarn_function, yeccpars2_46_/1}).
-compile({nowarn_unused_function, yeccpars2_46_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 68).
yeccpars2_46_(__Stack0) ->
    [
        begin
            []
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_47_/1}).
-dialyzer({nowarn_function, yeccpars2_47_/1}).
-compile({nowarn_unused_function, yeccpars2_47_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 65).
yeccpars2_47_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            merge([___1, #{type => scenario, steps => ___2, examples => ___3}])
        end
        | __Stack
    ].

-compile({inline, yeccpars2_48_/1}).
-dialyzer({nowarn_function, yeccpars2_48_/1}).
-compile({nowarn_unused_function, yeccpars2_48_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 68).
yeccpars2_48_(__Stack0) ->
    [
        begin
            []
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_50_/1}).
-dialyzer({nowarn_function, yeccpars2_50_/1}).
-compile({nowarn_unused_function, yeccpars2_50_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 71).
yeccpars2_50_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            ___2
        end
        | __Stack
    ].

-compile({inline, yeccpars2_51_/1}).
-dialyzer({nowarn_function, yeccpars2_51_/1}).
-compile({nowarn_unused_function, yeccpars2_51_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 69).
yeccpars2_51_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            [___1 | ___2]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_52_/1}).
-dialyzer({nowarn_function, yeccpars2_52_/1}).
-compile({nowarn_unused_function, yeccpars2_52_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 64).
yeccpars2_52_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            merge([___1, #{type => scenario, steps => ___2}])
        end
        | __Stack
    ].

-compile({inline, yeccpars2_53_/1}).
-dialyzer({nowarn_function, yeccpars2_53_/1}).
-compile({nowarn_unused_function, yeccpars2_53_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 73).
yeccpars2_53_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            merge([#{name => value(___1)}, ___2])
        end
        | __Stack
    ].

-compile({inline, yeccpars2_54_/1}).
-dialyzer({nowarn_function, yeccpars2_54_/1}).
-compile({nowarn_unused_function, yeccpars2_54_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 48).
yeccpars2_54_(__Stack0) ->
    [
        begin
            #{}
        end
        | __Stack0
    ].

-compile({inline, yeccpars2_55_/1}).
-dialyzer({nowarn_function, yeccpars2_55_/1}).
-compile({nowarn_unused_function, yeccpars2_55_/1}).
-file("/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 46).
yeccpars2_55_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            merge([___1, #{name => value(___2)}, ___3])
        end
        | __Stack
    ].

-file(
    "/Users/alind/Developer/Code/cucumis/src/cucumis_gherkin_parser.yrl", 177
).
