(require 'ert)


(ert-deftest extract-mvn-dependencies-when-scope-is-compile ()
       (should (-contains?
                (jx/get-mvn-project-dependencies
                 "[INFO] +- org.jx:lib:jar:1.0-SNAPSHOT:compile")
                "org.jx:lib:jar:1.0-SNAPSHOT:compile")))


(ert-deftest extract-mvn-dependencies-when-scope-is-test ()
       (should (-contains?
                (jx/get-mvn-project-dependencies
                 "[INFO] +- junit:junit:jar:4.10:test")
                "junit:junit:jar:4.10:test")))

(ert-deftest extract-mvn-dependencies-when-scope-is-test-with-message ()
       (should (-contains?
                (jx/get-mvn-project-dependencies
                 "[INFO] +- junit:junit:jar:4.10:test (scope not updated to compile)")
                "junit:junit:jar:4.10:test")))
