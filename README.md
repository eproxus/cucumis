# Cucumis

A Cucumber and Gherkin implementation for Erlang.

## Features

- **Full Gherkin syntax support**
    - [X] _Feature_, _Rule_, _Scenario_, and _Background_
    - [X] _Scenario Outline_ and _Examples_
    - [X] Tags
    - [X] Data tables and doc strings
    - [ ] Clear and readable parser error messages
- **Easy and flexible step definitions**
    - [X] Easy regex capture groups for step arguments
    - [ ] Access to step arguments, such as doc strings and tables
- **Test case execution**
    - [X] Supports multiple, composable step definition implementations
        - [X] Configurable at runtime
        - [ ] Configurable in `sys.config`
    - [X] _Background_ steps for rules executes for each scenario
    - [ ] Common test integration
    - [ ] EUnit integration
    - [ ] Test case traceability
        - [ ] Full step log
        - [ ] Original line numbers reported for failing steps

## Parsing

```erlang
2> cucumis:parse("test/files/guess_the_word.feature").
#{name => <<"Guess the word">>,
  rules =>
      [#{scenarios =>
             [#{name => <<"Maker starts a game">>,type => scenario,
                steps =>
                    [{'when',<<"the Maker starts a game">>,[]},
                     {then,<<"the Maker waits for a Breaker to join">>,[]}]},
              #{name => <<"Breaker joins a game">>,type => scenario,
                steps =>
                    [{given,<<"the Maker has started a game with the word \"silky\"">>,
                            []},
                     {'when',<<"the Breaker joins the Maker's game">>,[]},
                     {then,<<"the Breaker must guess a word with 5 characters">>,
                           []}]}]}]}
```

## Running Feature Tests

### EUnit

TBD

### Common Test

TBD

### Manual

TBD
