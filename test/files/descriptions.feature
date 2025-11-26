Feature: Highlander
    Feature description.

    Rule: There can be only One
        Rule description.

    Example: Only One -- More than one alive
        Example one description.

        Given there are 3 ninjas
        And there are more than one ninja alive
        When 2 ninjas meet, they will fight
        Then one ninja dies (but not me)
        And there is one ninja less alive

    Example: Only One -- One alive
        Example two description.
        Given there is only 1 ninja alive
        Then they will live forever ;-)
