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

;; Used to fix dependencies duplicate
(ert-deftest dependencies-should-be-always-overwritten ()
  (jx/update-dependencies '("org:hibernate:jar:3.2.5:compile"))
  (jx/update-dependencies '("junit:junit:jar:4.10:test" "org:log4j:jar:1.10:compile"))
  (should (equal (cdr (assoc :dependencies jx/default-config))
                 '("junit:junit:jar:4.10:test" "org:log4j:jar:1.10:compile"))))
