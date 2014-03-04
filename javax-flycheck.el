(require 'flycheck)

(defvar jx/mvn-repo-path "/home/nchapon/opt/m2_repo")

(defvar jx/ecj-path "/home/nchapon/opt/bin/ecj-4.3.1.jar")

(defvar jx/project-config-filename ".javax-project.el"
  "Project configuration file")

;; Default config file
(setq jx/default-config
  '((:source . "1.7")
    (:target . "1.7")
    (:options . "-warn:+over-ann,uselessTypeCheck -proceedOnError -maxProblems 100")
    (:dependencies . ())
    (:lib-paths . ("target/classes" "target/test-classes"))))


(defun jx/get-mvn-metadata (dependency n)
  "Get maven metadata"
  (nth n (split-string dependency ":")))

(defun jx/get-mvn-groupid (dependency)
  (replace-regexp-in-string "\\." "/" (jx/get-mvn-metadata dependency 0)))

(defun jx/get-mvn-artifactid (dependency)
  (jx/get-mvn-metadata dependency 1))

(defun jx/get-mvn-version (dependency)
  (jx/get-mvn-metadata dependency 3))

(defun jx/get-mvn-type (dependency)
  (jx/get-mvn-metadata dependency 2))

(defun jx/get-mvn-artifact-name (d)
  "Get maven artifact name from d"
  (format "%s-%s.%s"
    (jx/get-mvn-artifactid d)
    (jx/get-mvn-version d)
    (jx/get-mvn-type d)))

(defun jx/get-mvn-depency-path (dependency)
  "Get maven dependency path from DEPENDENCY"
  (format "%s/%s/%s/%s/%s"
          jx/mvn-repo-path
          (jx/get-mvn-groupid dep)
          (jx/get-mvn-artifactid dep)
          (jx/get-mvn-version dep)
          (jx/get-mvn-artifact-name dep)))

(defun jx/build-classpath (dependencies)
  "Build classpath from maven DEPENDENCIES"
  (let ((cp '("rt.jar")))
    (dolist (dep dependencies)
      (push (jx/get-mvn-depency-path dep) cp))
    cp))

(defun jx/mvn-current-project-dir ()
  "Returns current maven project directory"
  (interactive)
  (locate-dominating-file (buffer-file-name) "pom.xml"))

(defun jx/expand-project-config-file ()
  "Expand project file name with project directory"
  (expand-file-name
   jx/project-config-filename
   (jx/mvn-current-project-dir)))

(defun jx/read-config-file ()
  "Read config file"
  (interactive)
  (save-excursion
    (let ((buf (find-file-noselect (jx/expand-project-config-file))))
      (set-buffer buf)
      (read (buffer-string)))))

(defun jx/classpath ()
  "Concatenate dependencies managed by maven and project classes
build directoriies"
  (-concat
   (jx/build-classpath
    (cdr (assoc :dependencies (jx/read-config-file))))
   (-map
    (lambda (path) (expand-file-name path (jx/project-dir)))
    (cdr (assoc :lib-paths (jx/read-config-file))))))

(defun jx/get-classpath ()
  "Get full classpath, all entries are separated by Unix default
separator ':'"
  (interactive)
  (mapconcat 'identity (jx/classpath) ":"))


(defvar jx/mvn-dependency-pattern "\\[INFO\\] .* \\([0-9A-Za-z.-]+\\):\\([0-9A-Za-z.-]+\\):\\(jar\\):\\([0-9A-Za-z.-]+\\):\\(test\\|compile\\)$" "MATCH Dependencies regexp")


(defun jx/match-dependency (&optional string)
    "Return the list of all deps info matched in last search.
     STRING is optionally what was given to `string-match'."
    (let ((n-matches (1- (/ (length (match-data)) 2))))
      (rest (mapcar (lambda (i) (match-string i string))
                    (number-sequence 0 n-matches)))))

(defun jx/mvn-dependency-tree-command ()
  "Returns mvn dependency:tree output"
  (interactive)
  (shell-command-to-string
   (format "cd %s; mvn dependency:tree"
           (locate-dominating-file (buffer-file-name) "pom.xml"))))

(defun jx/get-mvn-project-dependencies ()
  "Get project dependencies in mvn format
   <groupid>:<artifactid>:<type>:<version>:<scope>"
  (interactive)
  (let ((mvn-output (jx/mvn-dependency-tree-command))
        (deps '())
        (start 0))
    (while (string-match jx/mvn-dependency-pattern mvn-output start)
      (push (mapconcat 'identity (jx/match-dependency mvn-output) ":") deps)
      (setq start (match-end 0)))
    deps))

(defun jx/dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (pp varlist buf)
      (pp-buffer)
      (save-buffer)
      (kill-buffer))))

(defun jx/update-dependencies (dependencies)
  "Update project dependencies"
  (let ((deps (cdr (assoc :dependencies jx/default-config))))
    (setf (cdr (assoc :dependencies jx/default-config)) (append deps dependencies))))

(defun jx/update-config ()
  "Create project-file"
  (interactive)
  (jx/update-dependencies (jx/get-mvn-project-dependencies))
  (jx/dump-vars-to-file jx/default-config
                     (jx/expand-project-config-file)))

;;; Define Flycheck Checker
(when (featurep 'flycheck-autoloads)
  (flycheck-define-checker java-syntax
    "Check syntax of java code using ecj compiler"
    :command   ("java"
                "-Xms128m"
                "-Xmx128m"
                "-jar" (eval jx/ecj-path)
                "-d" "none"
                "-1.7"
                "-Xemacs"
                "-cp" (eval (jx/get-classpath))
                source)
    :error-patterns
    ((warning line-start (file-name) ":" line
              ": warning:" (message) line-end)
     (error line-start (file-name) ":" line
            ": error:" (message) line-end))
    :modes java-mode)

  (add-to-list 'flycheck-checkers 'java-syntax))


(provide 'javax-flycheck)
;;; javax-flycheck ends here
