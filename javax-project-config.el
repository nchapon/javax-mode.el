(defcustom jx/mvn-repo-path "~/.m2"
  "The path to the local Maven repository"
  :group 'javax
  :type 'string
  :safe 'stringp)

(defcustom jx/ecj-path "~/bin/ecj.jar"
  "The full path to Java Eclipse Compiler"
  :group 'javax
  :type 'string
  :safe 'stringp)


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

(defun jx/mvn-artifact-name (d)
  "Get maven artifact name for dependency D"
  (format "%s-%s.%s"
    (jx/get-mvn-artifactid d)
    (jx/get-mvn-version d)
    (jx/get-mvn-type d)))

(defun jx/mvn-artifact-source-name (d)
  "Get maven artifact source name for dependency D"
  (format "%s-%s-sources.%s"
    (jx/get-mvn-artifactid d)
    (jx/get-mvn-version d)
    (jx/get-mvn-type d)))


(defun jx/get-mvn-dependency-path (callback dependency)
  "Get maven dependency path from DEPENDENCY"
  (format "%s/%s/%s/%s/%s"
          jx/mvn-repo-path
          (jx/get-mvn-groupid dependency)
          (jx/get-mvn-artifactid dependency)
          (jx/get-mvn-version dependency)
          (funcall callback dependency)))


(defun jx/build-classpath (dependencies)
  "Build classpath from maven DEPENDENCIES"
  (let ((cp '()))
    (dolist (dep dependencies)
      (push (jx/get-mvn-dependency-path 'jx/mvn-artifact-name dep) cp))
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


(defun jx/get-source  (lib)
  "DOCSTRING"
  (let ((src (jx/get-mvn-dependency-path 'jx/mvn-artifact-source-name lib)))
    (when (file-exists-p src)
      (make-directory
       (expand-file-name
        (file-name-sans-extension src)
        (format "%s.javax-sources" (jx/project-dir)))) ;; To refactor
      ;; (shell-command (format "unzip -d %s.javax-sources %s" (jx/project-dir) src))
      )))

(defun jx/src-libs ()
 "Get source code for dependencies"
 (interactive)
 (dolist (lib (cdr (assoc :dependencies (jx/read-config-file))))
   (jx/get-source lib)))

(defvar jx/mvn-dependency-pattern "\\[INFO\\] .* \\([0-9A-Za-z.-]+\\):\\([0-9A-Za-z.-]+\\):\\(jar\\):\\([0-9A-Za-z.-]+\\):\\(test\\|compile\\|provided\\)" "MATCH Dependencies regexp")


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

(defun jx/get-mvn-project-dependencies (mvn-output)
  "Get project dependencies in mvn format
   <groupid>:<artifactid>:<type>:<version>:<scope>"
  (interactive)
  (let ((deps '())
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
  (setf (cdr (assoc :dependencies jx/default-config)) dependencies))

(defun jx/update-config ()
  "Create project-file"
  (interactive)
  (jx/update-dependencies (jx/get-mvn-project-dependencies (jx/mvn-dependency-tree-command)))
  (jx/dump-vars-to-file jx/default-config
                     (jx/expand-project-config-file)))


(provide 'javax-project-config)
;;; javax-flycheck ends here
