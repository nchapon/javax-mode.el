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
     (:classpath "M2_REPO/junit/junit/4.11/junit-4.11.jar" "M2_REPO/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar" "M2_REPO/org/assertj/assertj-core/1.5.0/assertj-core-1.5.0.jar" "M2_REPO/org/slf4j/slf4j-api/1.7.5/slf4j-api-1.7.5.jar" "M2_REPO/ch/qos/logback/logback-classic/1.1.0/logback-classic-1.1.0.jar" "M2_REPO/ch/qos/logback/logback-core/1.1.0/logback-core-1.1.0.jar")
     (:lib-paths "target/classes" "target/test-classes"))
    """
