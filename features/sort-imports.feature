Feature: Sort imports when organize Imports

  Background:
    Given I have a maven project "jx" in "tmp"

  Scenario: Should sort used imports
    When I open file "tmp/src/main/java/org/jx/App.java"
    And I clear the buffer
    And I insert:
    """
    package org.jx;

    import org.jx.ParentClass;
    import org.jx.ClassA;
    import org.jx.Annotation;
    import org.jx.ClassB;

    public class App extends ParentClass {

        @Annotation
        public void doSomething(){
            ClassA.doSomething();
            new ClassB();
        }
    }
    """
    And I go to end of buffer
    And I press "C-c C-o"
    Then I should see:
    """
    import org.jx.Annotation;
    import org.jx.ClassA;
    import org.jx.ClassB;
    import org.jx.ParentClass;
    """


 Scenario: Should group imports with a default pattern : java, javax, org, com and others
    When I open file "tmp/src/main/java/org/jx/system/Calculator.java"
    And I go to end of buffer
    And I press "C-c C-o"
    Then I should see:
    """
    import java.util.ArrayList;
    import java.util.List;

    import org.slf4j.Logger;
    import org.slf4j.LoggerFactory;
    """
