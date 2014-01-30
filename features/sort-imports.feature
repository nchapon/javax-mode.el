Feature: Sort imports when organize Imports

  Background:
    Given I have a maven project "jx" in "tmp"
    And I have a java-file "tmp/src/main/java/org/jx/App.java"
    And I open file "tmp/src/main/java/org/jx/App.java"
    And I clear the buffer

  Scenario: Should sort used imports
    When I insert:
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
