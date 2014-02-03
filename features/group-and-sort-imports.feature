Feature: Group and order imports when organize Imports

  Background:
    Given I have a maven project "jx" in "tmp"


  Scenario: Should group and sort imports when I have only one group
    When I open file "tmp/src/main/java/org/jx/App.java"
    And I clear the buffer
    And I insert:
    """
    package org.jx;

    import org.jx.ParentClass;
    import org.jx.ClassA;
    import org.lang.Annotation;
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
    package org.jx;

    import org.jx.ClassA;
    import org.jx.ClassB;
    import org.jx.ParentClass;
    import org.lang.Annotation;

    public class App extends ParentClass {

        @Annotation
        public void doSomething(){
            ClassA.doSomething();
            new ClassB();
        }
    }
    """

  Scenario: Should group and sort all imports
    When I open file "tmp/src/main/java/org/jx/App.java"
    And I clear the buffer
    And I insert:
    """
    package org.jx;

    import fr.jx.ParentClass;
    import org.jx.ClassA;
    import java.lang.Annotation;
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
    package org.jx;

    import java.lang.Annotation;

    import org.jx.ClassA;
    import org.jx.ClassB;

    import fr.jx.ParentClass;

    public class App extends ParentClass {

        @Annotation
        public void doSomething(){
            ClassA.doSomething();
            new ClassB();
        }
    }
    """


 Scenario: Should keep one blank lines before and after when organize import two or more times
    When I open file "tmp/src/main/java/org/jx/system/Calculator.java"
    And I go to end of buffer
    And I press "C-c C-o"
    And I press "C-c C-o"
    Then I should see:
    """
    package org.jx.system;

    import java.util.ArrayList;
    import java.util.List;

    import org.slf4j.Logger;
    import org.slf4j.LoggerFactory;

    public class Calculator {

        private static Logger logger = LoggerFactory.getLogger(Calculator.class);

        public int sum (int a, int b) {
            logger.info("Sum {} {}",a,b);
            List list = new ArrayList();
            return a+b;
        }

    }
    """
