(defun jx/mvn (command)
  "Runs mvn COMMAND"
  (shell-command-to-string
   (format
    "cd %s; mvn %s"
    (locate-dominating-file (buffer-file-name) "pom.xml")
    command)))



(provide 'javax-mvn)
