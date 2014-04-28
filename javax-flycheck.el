(require 'javax-project-config)
(require 'flycheck)


;;; Define Java Checker
(flycheck-define-checker java
  "Check syntax of java code using ecj compiler"
  :command   ("java"
              "-Xms128m"
              "-Xmx128m"
              "-jar" (eval jx/ecj-path)
              "-d" "none"
              "-1.8"
              "-Xemacs"
              "-cp" (eval (jx/get-classpath))
              source)
  :error-patterns
  ((warning line-start (file-name) ":" line
            ": warning:" (message) line-end)
   (error line-start (file-name) ":" line
          ": error:" (message) line-end))
  :modes java-mode)

(add-to-list 'flycheck-checkers 'java)


(defun jx/flycheck-java-setup ()
  "Gets JAVA exccutable from USER path and setup flycheck."
   (setq flycheck-java-executable
         (s-trim (shell-command-to-string "which java"))))

(add-hook 'flycheck-mode-hook #'jx/flycheck-java-setup)

(provide 'javax-flycheck)
;;; javax-flycheck ends here
