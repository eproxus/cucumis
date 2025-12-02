# Language comment - this is a comment
@feature_tag @smoke
Feature: Complete Gherkin Syntax Example
  This is a multi-line description of the feature.
  It can span several lines and provides context
  about what this feature does.

  Background:
    Given the system is initialized
    And the database is empty

  @scenario_tag @regression
  Scenario: Basic scenario with all step types
    Given a user named "Alice"
    And she has an account
    When she logs into the system
    And she navigates to the dashboard
    Then she should see a welcome message
    But she should not see admin options

  @outline_tag
  Scenario Outline: Parameterized scenario with examples
    Given there are <initial> items in the cart
    When the user adds <added> more items
    Then the cart should contain <total> items

    Examples:
      | initial | added | total |
      |       5 |     3 |     8 |
      |      10 |     0 |    10 |
      |       0 |     7 |     7 |

    # Edge cases
    Examples:
      | initial | added | total |
      |     100 |     1 |   101 |

  Scenario: Scenario with data table
    Given the following users exist:
      | name  | email           | role  |
      | Alice | alice@test.com  | admin |
      | Bob   | bob@test.com    | user  |
      | Carol | carol@test.com  | guest |
    When the system sends notifications
    Then all users should be notified

  Scenario: Scenario with doc string
    Given a blog post with the following content:
      """
      This is a multi-line string.
      It preserves formatting and newlines.

      It can contain special characters: @#$%
      """
    When the post is published
    Then it should appear on the homepage

  @rule_tag
  Rule: Business rule grouping scenarios
    Rules group related scenarios that follow a specific business rule.

    Background:
      Given the business rule context is set

    @rule_scenario_tag
    Example: First example under rule
      Given a condition specific to this rule
      When an action occurs
      Then the rule is enforced

    Example: Second example under rule
      Given another condition
      Then the rule still applies
