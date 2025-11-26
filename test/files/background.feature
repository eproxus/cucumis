Feature: Multiple site support
    Only blog owners can post to a blog, except administrators,
    who can post to all blogs.

    Background:
        Given a global administrator named "Greg"
        And a blog named "Greg's anti-tax rants"
        And a customer named "Dr. Bill"
        And a blog named "Expensive Therapy" owned by "Dr. Bill"

    Scenario: Dr. Bill posts to his own blog
        Given I am logged in as Dr. Bill
        When I try to post to "Expensive Therapy"
        Then I should see "Your article was published."

    Scenario: Dr. Bill tries to post to somebody else's blog, and fails
        Given I am logged in as Dr. Bill
        When I try to post to "Greg's anti-tax rants"
        Then I should see "Hey! That's not your blog!"

    Scenario: Greg posts to a client's blog
        Given I am logged in as Greg
        When I try to post to "Expensive Therapy"
        Then I should see "Your article was published."


    Rule: Users are notified about overdue tasks on first use of the day
        Background:
            Given I have overdue tasks

        Example: First use of the day
            Given I last used the app yesterday
            When I use the app
            Then I am notified about overdue tasks

        Example: Already used today
            Given I last used the app earlier today
            When I use the app
            Then I am not notified about overdue tasks
