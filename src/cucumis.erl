-module(cucumis).

-behaviour(application).

% API
-export([parse/1]).
-export([parse_file/1]).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- API -----------------------------------------------------------------------

parse(Content) -> cucumis_gherkin_parser:content(Content).

parse_file(File) -> cucumis_gherkin_parser:file(File).

%--- Callbacks -----------------------------------------------------------------

start(_StartType, _StartArgs) -> cucumis_sup:start_link().

stop(_State) -> ok.
