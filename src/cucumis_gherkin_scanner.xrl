Definitions.

S = (\s|\t)
Tag = @[a-zA-Z._-]+

Rules.

% \n : {token, {newline, TokenLoc, TokenChars}}.
{S}*@.* : {token, {tags, TokenLoc, tags(TokenChars)}}.
\n : skip_token.
% Skip comments
{S}*#.* : skip_token.
{S}*Feature\:.* : {token, statement("Feature", TokenLoc, TokenChars)}.
{S}*Background\:.* : {token, statement("Background", TokenLoc, TokenChars)}.
{S}*Rule\:.* : {token, statement("Rule", TokenLoc, TokenChars)}.
{S}*Scenario\sOutline\:.* : {token, statement("Scenario Outline", TokenLoc, TokenChars)}.
{S}*Scenario\:.* : {token, statement("Scenario", TokenLoc, TokenChars)}.
{S}*Example\:.* : {token, statement("Example", TokenLoc, TokenChars)}.
{S}*Examples\:.* : {token, statement("Examples", TokenLoc, TokenChars)}.
{S}*And\s.* : {token, statement("And", TokenLoc, TokenChars)}.
{S}*But\s.* : {token, statement("But", TokenLoc, TokenChars)}.
{S}*When\s.* : {token, statement("When", TokenLoc, TokenChars)}.
{S}*Then\s.* : {token, statement("Then", TokenLoc, TokenChars)}.
{S}*Given\s.* : {token, statement("Given", TokenLoc, TokenChars)}.
{S}*""" : {token, {doc_quote, TokenLoc, TokenChars}}.
{S}*``` : {token, {doc_tick, TokenLoc, TokenChars}}.
{S}*\|.* : {token, {table_row, TokenLoc, TokenChars}}.
.* : {token, {text, TokenLoc, TokenChars}}.

Erlang code.
-export([content/1]).
-export([file/1]).

%--- API -----------------------------------------------------------------------

content(Content) when is_binary(Content) ->
    {ok, Tokens, _EndLoc} = ?MODULE:string(binary_to_list(Content)),
    Tokens;
content(Content) when is_list(Content) ->
    content(iolist_to_binary(Content)).

file(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} -> content(Content);
        {error, enoent} -> error({file_not_found, Filename});
        {error, Reason} -> error({could_not_load_file, Filename, Reason})
    end.

%--- Internal ------------------------------------------------------------------

statement(Prefix, Loc, String) ->
    {
        keyword_to_atom(Prefix),
        Loc,
        string:trim(b(string:prefix(string:trim(String), Prefix)), both, ": ")
    }.

keyword_to_atom(Keyword) ->
    binary_to_atom(b(string:replace(string:to_lower(Keyword), " ", "_"))).

b(IOList) -> iolist_to_binary(IOList).

tags(String) ->
    [
        T
     || <<"@", T/bytes>> <- re:split(string:trim(String), "[[:space:]]+"),
        T =/= <<>>
    ].
