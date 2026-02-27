-module(tables_steps).

-export([init/0]).
-export([steps/0]).
-export([result/1]).

%--- Definitions ---------------------------------------------------------------

init() -> #{cucumbers => [], twitter => [], airports => []}.

steps() ->
    [
        % Cucumbers
        {~"there are <start> cucumbers", fun cucumber_start/3},
        {~"I eat <eat> cucumbers", fun cucumber_eat/3},
        {~"I should have <left> cucumbers", fun cucumber_left/3},
        % Twitter
        {~"the following users exist", fun twitter_users/4},
        % Airports
        {~"some airport locations", fun airport_locations/3}
    ].

result(State) -> maps:map(fun(_, L) -> lists:reverse(L) end, State).

%--- Steps ---------------------------------------------------------------------

% Cucumbers

% _Match = #{start := 12}
cucumber_start(_Match, #{cucumbers := S} = State, Env) ->
    {success, State#{cucumbers := [start | S]}, Env}.

cucumber_eat(_Match, #{cucumbers := S} = State, Env) ->
    {success, State#{cucumbers := [eat | S]}, Env}.

cucumber_left(_Match, #{cucumbers := S} = State, Env) ->
    {success, State#{cucumbers := [left | S]}, Env}.

% Twitter

twitter_users(_Match, #{twitter := S} = State, Env, [Users]) ->
    {
        success,
        State#{twitter := [{users, [N || #{~"name" := N} <- Users]} | S]},
        Env
    }.

% Airports

airport_locations(_Match, #{airports := S} = State, Env) ->
    {success, State#{airports := [locations | S]}, Env}.
