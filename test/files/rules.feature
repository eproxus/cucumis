# -- FILE: features/gherkin.rule_example.feature
Feature: Highlander

    Rule: There can be only One

        Example: Only One -- More than one alive
            Given there are 3 ninjas
            And there are more than one ninja alive
            When 2 ninjas meet, they will fight
            Then one ninja dies (but not me)
            And there is one ninja less alive

        Example: Only One -- One alive
            Given there is only 1 ninja alive
            Then they will live forever ;-)

    Rule: There can be Two (in some cases)

        Example: Two -- Dead and Reborn as Phoenix
            Then two ninjas exist
