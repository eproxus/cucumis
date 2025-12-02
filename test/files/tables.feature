Feature: a

    Scenario Outline: eating
        Given there are <start> cucumbers
        When I eat <eat> cucumbers
        Then I should have <left> cucumbers

        Examples:
            | start | eat | left |
            |    12 |   5 |    7 |
            |    20 |   5 |   15 |

    Scenario: Create users
        Given the following users exist:
            | name   | email              | Twitter Handle  |
            | Aslak  | aslak@cucumber.io  | @aslak_hellesoy |
            | Julien | julien@cucumber.io | @jbpros         |
            | Matt   | matt@cucumber.io   | @mattwynne      |

    Scenario: Row keys
        Given some airport locations:
            |      |       lat |         lon |
            | KMSY | 29.993333 |  -90.258056 |
            | KSFO | 37.618889 | -122.375000 |
            | KSEA | 47.448889 | -122.309444 |
            | KJFK | 40.639722 |  -73.778889 |
