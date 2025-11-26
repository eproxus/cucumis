Feature: Blog

  Scenario: First blog post
    Given a blog post named "Foobar" with Markdown body
    ```
    Some Title, Eh?
    ===============
    Here is the first paragraph of my blog post. Lorem ipsum dolor sit amet,
    consectetur adipiscing elit.
    ```

  Scenario: Second blog post
    Given a blog post named "Random" with Markdown body
    """
    * Foo
        * Bar
        * Baz

    Code:

        some code
    """
    Then indentation and newlines should be kept
