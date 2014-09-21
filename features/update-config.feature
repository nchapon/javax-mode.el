Feature: Configure / Init a JX project from a maven simple project.

  Background:
    Given I have a maven project "simple-project" in "tmp"

  Scenario: If no jx-project file create it
    When I open file "tmp/src/main/java/org/jx/App.java"
    And I go to end of buffer
    And I press "M-x jx/update-config C-j 1.7"
    Then there should exist a file called ".javax-project.el" with this content:
    """
    ((:source . "1.7")
     (:target . "1.7")
     (:options . "-warn:+over-ann,uselessTypeCheck -proceedOnError -maxProblems 100")
     (:dependencies "ch.qos.logback:logback-core:jar:1.1.0:compile" "ch.qos.logback:logback-classic:jar:1.1.0:compile" "org.slf4j:slf4j-api:jar:1.7.5:compile" "org.assertj:assertj-core:jar:1.5.0:test" "org.hamcrest:hamcrest-core:jar:1.3:test" "junit:junit:jar:4.11:test")
     (:lib-paths "target/classes" "target/test-classes"))
    """
