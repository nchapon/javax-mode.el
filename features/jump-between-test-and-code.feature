Feature: Jump between source code and test

  Background:
    Given I have a maven project "jx" in "tmp"

  Scenario: Jump to test case from source code
    When I open file "tmp/src/main/java/org/jx/App.java"
    And I press "M-<"
    And I press "C-c C-t"
    Then the file should be named "AppTest.java"

  Scenario: Jump to source code from test class
    When I open file "tmp/src/test/java/org/jx/AppTest.java"
    And I press "M->"
    And I press "C-c C-t"
    Then the file should be named "App.java"
