# Cucumis

A Cucumber and Gherkin implementation for Erlang.

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
