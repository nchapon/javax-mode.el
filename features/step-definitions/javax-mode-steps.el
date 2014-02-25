;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I open file \"\\(.+\\)\"$"
       (lambda (filename)
         (setq default-directory javax-mode-root-path)
         (find-file filename)))

(Given "^I have a maven project \"\\([^\"]+\\)\" in \"\\([^\"]+\\)\"$"
       (lambda (project-name dir-name)
         (setq default-directory javax-mode-root-path)

         ;; delete old directory
         (when (file-exists-p dir-name)
           (delete-directory dir-name t))

         ;; copy jx content from java-templates
         (copy-directory
          (expand-file-name "simple-project" javax-projects-path)
          dir-name)))

(Given "^I have a java-file \"\\([^\"]+\\)\"$"
     (lambda (file-name)
       (setq default-directory javax-mode-root-path)
       (find-file file-name)
       (save-buffer)
       (kill-buffer)))

(Then "^the file should be named \"\\([^\"]+\\)\"$"
      (lambda (file-name-postfix)
        (assert (s-ends-with? file-name-postfix (buffer-file-name)) nil "Expected %S to end with %S" (buffer-file-name) file-name-postfix)))
