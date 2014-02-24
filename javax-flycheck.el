(require 'flycheck)

(defvar mvn-repo-path "/home/nchapon/opt/m2_repo")

(defvar ecj-jar-path "/home/nchapon/opt/bin/ecj-4.3.1.jar")

(defvar project-file ".javax-project.el"
  "Project configuration file")

(defun get-mvn-metadata (dependency n)
  "Get maven metadata"
  (nth n (split-string dependency ":")))

(defun get-mvn-groupid (dependency)
  (replace-regexp-in-string "\\." "/" (get-mvn-metadata dependency 0)))

(defun get-mvn-artifactid (dependency)
  (get-mvn-metadata dependency 1))

(defun get-mvn-version (dependency)
  (get-mvn-metadata dependency 3))

(defun get-mvn-type (dependency)
  (get-mvn-metadata dependency 2))

(defun get-mvn-artifact-name (d)
  "Get maven artifact name from d"
  (format "%s-%s.%s"
    (get-mvn-artifactid d)
    (get-mvn-version d)
    (get-mvn-type d)))

(defun get-mvn-depency-path (dependency)
  "Get maven dependency path from DEPENDENCY"
  (format "%s/%s/%s/%s/%s"
          mvn-repo-path
          (get-mvn-groupid dep)
          (get-mvn-artifactid dep)
          (get-mvn-version dep)
          (get-mvn-artifact-name dep)))

(defun classpath (deps)
  "Build classpath from DEPS"
  (interactive)
  (let ((cp '("rt.jar")))
    (dolist (dep deps)
      (push (get-mvn-depency-path dep) cp))
    cp))

(defun javax-project-dir ()
  "Returns Java Project Directory"
  (interactive)
  (locate-dominating-file (buffer-file-name) "pom.xml"))


(defun expand-project-file ()
  "Expand project file name with project directory"
  (expand-file-name project-file (javax-project-dir)))

(defun read-project-config ()
  "Read config file"
  (interactive)
  (save-excursion
    (let ((buf (find-file-noselect (expand-project-file)))
          (project-config nil))
      (set-buffer buf)
      (read (buffer-string)))))
;; PB buffer-name stills expand-project-file !!!

(defun init-classpath ()
  (classpath
   (cdr (assoc :dependencies (read-project-config)))))

;; TODO refactoring
(defun get-build-classpath ()
  "Compile Buffer"
  (interactive)
  (mapconcat 'identity (init-classpath) ":"))


(when (featurep 'flycheck-autoloads)
  (flycheck-define-checker java-syntax
    "Check syntax of java code using ecj compiler"
    :command   ("java"
                "-Xms128m"
                "-Xmx128m"
                "-jar" (eval ecj-jar-path)
                "-d" "none"
                "-1.7"
                "-Xemacs"
                "-cp" (eval (get-build-classpath))
                source)
    :error-patterns
    ((warning line-start (file-name) ":" line
              ": warning:" (message) line-end)
     (error line-start (file-name) ":" line
            ": error:" (message) line-end))
    :modes java-mode)

  (add-to-list 'flycheck-checkers 'java-syntax))


(defvar mvn-dependency-pattern "\\[INFO\\] .* \\([0-9A-Za-z.-]+\\):\\([0-9A-Za-z.-]+\\):\\(jar\\):\\([0-9A-Za-z.-]+\\):\\(test\\|compile\\)$" "MATCH Dependencies regexp")


(defun match-dependency (&optional string)
    "Return the list of all deps info matched in last search.
     STRING is optionally what was given to `string-match'."
    (let ((n-matches (1- (/ (length (match-data)) 2))))
      (rest (mapcar (lambda (i) (match-string i string))
                    (number-sequence 0 n-matches)))))

(defun mvn-dependency-tree ()
  "Returns mvn dependency:tree output"
  (interactive)
  (shell-command-to-string
   (format "cd %s; mvn dependency:tree"
           (locate-dominating-file (buffer-file-name) "pom.xml"))))

(defun get-project-dependencies ()
  "Get project dependencies in mvn format
   <groupid>:<artifactid>:<type>:<version>:<scope>"
  (interactive)
  (let ((mvn-output (mvn-dependency-tree))
        (deps '())
        (start 0))
    (while (string-match mvn-dependency-pattern mvn-output start)
      (push (mapconcat 'identity (match-dependency mvn-output) ":") deps)
      (setq start (match-end 0)))
    deps))

(defun dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (pp varlist buf)
      (pp-buffer)
      (save-buffer)
      (kill-buffer))))

(setq default-java-project
  '((:source . "1.7")
    (:target . "1.7")
    (:options . "-warn:+over-ann,uselessTypeCheck -proceedOnError -maxProblems 100")
    (:dependencies . ())
    (:lib-paths . ("target/classes" "target/test-classes"))))


(defun update-project-dependencies (dependencies)
  "Update project dependencies"
  (let ((deps (cdr (assoc :dependencies default-java-project))))
    (setf (cdr (assoc :dependencies default-java-project)) (append deps dependencies))))

(defun update-project-config ()
  "Create project-file"
  (interactive)
  (update-project-dependencies (get-project-dependencies))
  (dump-vars-to-file default-java-project
                     (expand-project-file)))
