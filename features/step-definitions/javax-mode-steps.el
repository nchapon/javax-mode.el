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

         ;; copy project content from java-projects
         (copy-directory
          (expand-file-name project-name javax-projects-path)
          dir-name)
         ;; set javax-current-project
         (setq javax-current-project (expand-file-name dir-name default-directory))))

(Given "^I have a java-file \"\\([^\"]+\\)\"$"
     (lambda (file-name)
       (setq default-directory javax-mode-root-path)
       (find-file file-name)
       (save-buffer)
       (kill-buffer)))

(Then "^the file should be named \"\\([^\"]+\\)\"$"
      (lambda (file-name-postfix)
        (assert (s-ends-with? file-name-postfix (buffer-file-name)) nil "Expected %S to end with %S" (buffer-file-name) file-name-postfix)))


(Then "^there should exist a file called \"\\([^\"]+\\)\" with this content:$"
  (lambda (filename content)
    (let ((filepath (f-expand filename javax-current-project)))
      (with-temp-buffer
        (insert-file-contents filepath)
        (Then "I should see:" content)))))
