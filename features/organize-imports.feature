Feature: Organize Imports

  Background:
    Given I have a maven project "simple-project" in "tmp"
    And I have a java-file "tmp/src/main/java/org/jx/App.java"
    And I open file "tmp/src/main/java/org/jx/App.java"
    And I clear the buffer


  Scenario: Should remove unused import
    When I insert:
    """
    package org.jx;

    import org.jx.NotUsed;

    public class App {


    }
    """
    And I go to end of buffer
    And I press "C-c j o"
    Then I should not see:
    """
    import org.jx.NotUsed;
    """

  Scenario: Should not remove used imports
    When I insert:
    """
    package org.jx;

    import org.jx.Annotation;
    import org.jx.ClassA;
    import org.jx.ClassB;
    import org.jx.ParentClass;

    public class App extends ParentClass {

        @Annotation
        public void doSomething(){
            ClassA.doSomething();
            new ClassB();
        }
    }
    """
    And I go to end of buffer
    And I press "C-c j o"
    Then I should see:
    """
    import org.jx.Annotation;
    import org.jx.ClassA;
    import org.jx.ClassB;
    import org.jx.ParentClass;
    """

  Scenario: Should not remove static imports ending with *
    When I insert:
    """
    package org.jx;

    import static org.jx.StaticClass.*;

    public class App {

        public void doSomething(){
            callStaticMethod();
        }
    }
    """
    And I go to end of buffer
    And I press "C-c j o"
    Then I should see:
    """
    import static org.jx.StaticClass.*;
    """
