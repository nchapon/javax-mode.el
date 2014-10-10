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
    (:classpath . ())
    (:lib-paths . ("target/classes" "target/test-classes"))))



(defun jx/expand-path(path)
  "Expand classpath PATH with M2_REPO"
  (s-replace
   "M2_REPO"
   jx/mvn-repo-path
   path))

(defun jx/abbrev-path(path)
  "Abbrev classpath PATH with M2_REPO"
  (s-replace
   jx/mvn-repo-path
   "M2_REPO"
   path))

(defun jx/expand-classpath (paths)
  "Build classpath from classpath PATHS"
  (-map 'jx/expand-path paths))

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
   (jx/expand-classpath
    (cdr (assoc :classpath (jx/read-config-file))))
   (-map
    (lambda (path) (expand-file-name path (jx/project-dir)))
    (cdr (assoc :lib-paths (jx/read-config-file))))))

(defun jx/get-classpath ()
  "Get full classpath, all entries are separated by Unix default
separator ':'"
  (interactive)
  (mapconcat 'identity (jx/classpath) ":"))

(defun jx/mvn-build-classpath-command ()
  "Returns mvn dependency:build-classpath output"
  (interactive)
  (shell-command-to-string
   (format "cd %s; mvn dependency:build-classpath"
           (locate-dominating-file (buffer-file-name) "pom.xml"))))

(defun jx/get-mvn-classpath (output)
  "Get project dependencies from mvn OUTPUT"
  (interactive)
  (string-match "\\[INFO\\] Dependencies classpath:\n\\(.*.jar$\\)" output)
  (match-string 1 output))

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

(defun jx/update-config-param (param value)
  "Update config PARAM with VALUE"
  (interactive)
  (setf (cdr (assoc param jx/default-config)) value))

(defun jx/jdk-is-valid? (jdk)
  "Validate JDK version"
  (or
   (string= jdk "1.8")
   (string= jdk "1.7")
   (string= jdk "1.6")))

(defun jx/mvn-dependencies()
  "Maven project dependencies"
  (-map 'jx/abbrev-path
        (s-split ":" (jx/get-mvn-classpath (jx/mvn-build-classpath-command)))))

(defun jx/update-config (jdk)
  "Create project config file for JDK"
  (interactive "sPlease specify JDK version (1.6, 1.7 or 1.8) ? ")
  (if (not (jx/jdk-is-valid? jdk))
     (error "JDK %s is not valid you should use 1.6, 1.7 or 1.8" jdk))
  (jx/update-config-param :source jdk)
  (jx/update-config-param :target jdk)
  (jx/update-config-param :classpath (jx/mvn-dependencies))
  (jx/dump-vars-to-file jx/default-config
                     (jx/expand-project-config-file)))


(provide 'javax-project-config)
;;; javax-project-config ends here
