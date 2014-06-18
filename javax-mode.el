;;; javax-mode.el --- Minor mode to improve java experience in Emacs
(require 'cc-mode)
(require 'compile)
(require 'javax-project-config)
(require 'javax-flycheck)


(defcustom jx/mvn-build-command "mvn -f %spom.xml clean install"
  "Format string to run mvn build command. his format string
   should use '%s' to substitute the project maven root
   directory."
  :group 'javax
  :type 'string
  :safe 'stringp)

(defcustom jx/mvn-compile-command "cd %s; mvn -o compile"
  "Format string to run mvn compile command. This format string
   should use '%s' to substitute the project maven root
   directory."
  :group 'javax
  :type 'string
  :safe 'stringp)


(defcustom jx/group-import-order '("java" "javax" "org" "com")
  "Sort import pacakges order"
  :group 'javax
  :type 'list
  :safe 'listp)

(defun jx/project-dir ()
  "Returns root project dir for current buffer"
  (-if-let (git-project (locate-dominating-file (buffer-file-name) ".git"))
      git-project
    (error "Unable to locate root project dir !!!")))

(defun jx/mvn-project-dir ()
  "Returns maven project dir for current buffer"
  (-if-let (mvn-project (locate-dominating-file (buffer-file-name) "pom.xml"))
      mvn-project
    (error "Unable to locate maven pom.xml !!!")))

(defun jx/mvn-build ()
  "Builds current maven project"
  (interactive)
  (let* ((dir (file-name-as-directory (expand-file-name default-directory)))
         (found (file-exists-p (concat dir "pom.xml"))))
    (while (and (not found) (not (equal dir "/")))
      (setq dir (file-name-as-directory (expand-file-name (concat dir "..")))
            found (file-exists-p (concat dir "pom.xml"))))
    (if (not found)
        (message "No pom.xml found")
      (compile (read-from-minibuffer "Command: "
                                     (format jx/mvn-build-command dir) nil nil 'compile-history)))))

(defun jx/mvn-compile ()
  "Runs mvn test from a buffer file"
  (interactive)
  (compile
        (format
         jx/mvn-compile-command
         (jx/mvn-project-dir)
         (car(split-string (buffer-name) "\\.")))))

(defun jx/mvn-test ()
  "Runs mvn test from a buffer file"
  (interactive)
  (compile
        (format
         "cd %s; mvn -Dtest=%s test"
         (jx/mvn-project-dir)
         (car(split-string (buffer-name) "\\.")))))

;;; For maven 2/3 output
(add-to-list 'compilation-error-regexp-alist
             '("^.*?\\(/.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3))


(defun jx/path-for (package)
  "Convert PACKAGE name to real path."
  (interactive)
  (s-replace "." "/" package))


(defun jx/find-package ()
  "Find current buffer package"
  (interactive)
  (save-excursion
    (goto-char (point-min))
       (when (re-search-forward "\\(^package \\(.*\\);$\\)" nil t))
          (match-string-no-properties 2)))


(defun jx/find-symbol-package (symbol &optional default)
  "Find the package for the current SYMBOL, use DEFAULT if not found"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (format "\\(^import \\(.*\\)%s;$\\)" symbol) nil t)
          (match-string-no-properties 2)
        (cond ((s-blank? default) (jx/find-package))
              (t default))))))

;; (defun jx/electric-brace ()
;;   "Insert automatically close brace after 2 new lines."
;;   (interactive)
;;   (insert " {")
;;   (backward-char)
;;   (fixup-whitespace)
;;   (move-end-of-line 1)
;;   (indent-for-tab-command)
;;   (insert "\n\n")
;;   (insert "}")
;;   (indent-for-tab-command)
;;   (previous-line)
;;   (indent-for-tab-command))

(defun jx/symbol-at-point ()
  "Read symbol at point"
  (interactive)
  (let ((str (thing-at-point 'symbol)))
    str))

(defun jx/read-symbol-name (prompt callback &optional query)
  "Read symbol name."
  (let ((symbol-name (jx/symbol-at-point)))
    (cond
     ((not (or current-prefix-arg query (not symbol-name)))
      (funcall callback symbol-name))
     (t (funcall callback (read-from-minibuffer prompt))))))

(defun jx/find-file-from-archive (archive sourcefile)
  "Find SOURCEFILE from ARCHIVE, should be a ZIP or JAR file."
  (interactive)
  (with-current-buffer (find-file-noselect archive)
    (goto-char (point-min))
    (when (search-forward sourcefile nil t)
      (let ((filename (match-string 0)))
        (archive-view)))))

(defun jx/java-package? (package)
  "DOCSTRING"
  (interactive)
  (or
   (s-starts-with? "java" package)
   (s-starts-with? "javax" package)
   (s-starts-with? "com.sun" package)
   (s-starts-with? "org.w3c" package)))


(defun jx/find-archive-file-for (package)
  "Find archive file for PACKAGE"
  (interactive)
  (if (jx/java-package? package)
      (expand-file-name "src.zip" (getenv "JAVA_HOME"))
    (error "Archive not found for %s" package)))

(defun jx/java-src-handler (symbol)
  "Find source file for SYMBOL in Java Source Code"
  (let ((package (jx/find-symbol-package symbol "java.lang.")))
    (jx/find-file-from-archive
     (jx/find-archive-file-for package)
     (format "%s%s.java" (jx/path-for package) symbol))))

(defun jx/find-file (symbol)
  "Find java file from project root for SYMBOL"
  (let ((package (jx/find-symbol-package symbol)))
    (shell-command-to-string
    (format "find %s -iname %s.java -print0 | grep -FzZ %s"
            (jx/project-dir) symbol (jx/path-for package)))))

(defun jx/src-handler (symbol)
  "Create a handler to lookup java source code for SYMBOL"
  (let ((results (jx/find-file symbol)))
    (cond
     ((string= "" results) (jx/java-src-handler symbol))
     (t (find-file (first (split-string results)))))))

(defun jx/src (query)
  "Open java source file for the given QUERY.
Defaults to the symbol at point. With prefix arg or no symbol under
point, prompts for a var"
  (interactive "P")
  (jx/read-symbol-name "Class :" 'jx/src-handler query))

(defun jx/in-tests-p ()
  "Check whether the current file is a test file."
  (string-match-p "src/test/java" (buffer-file-name)))

(defun jx/test-for (package)
  "Returns the path of the the test file for a given PACKAGE."
  (format
   "%ssrc/test/java/%s/%sTest.java"
   (jx/mvn-project-dir)
   (jx/path-for package)
   (car (split-string (buffer-name) "\\.java"))))

(defun jx/implementation-for (package)
  "Returns the path of the the implementaion file for a given PACKAGE."
  (format
   "%ssrc/main/java/%s/%s.java"
   (jx/mvn-project-dir)
   (jx/path-for package)
   (car (split-string (buffer-name) "Test\\.java"))))

(defun jx/jump-to-test ()
  "Jump from implementation file to test."
  (interactive)
    (find-file (jx/test-for (jx/find-package))))

(defun jx/jump-to-implementation ()
  "Jump from test file to implementation."
  (find-file (jx/implementation-for (jx/find-package))))

(defun jx/jump-between-tests-and-code ()
  "Jump between tests and code"
  (interactive)
  (if (jx/in-tests-p)
       (jx/jump-to-implementation)
    (jx/jump-to-test)))

(defun jx/search-symbol (symbol)
  "Search symbol, should be case sensitive."
  (let ((case-fold-search nil))
    (re-search-forward (format "\\b%s\\b" symbol) nil t)))

(defun jx/clear-unused-imports (classname)
  "Clear unused imported classes."
  (goto-char (point-min))
  (jx/search-symbol classname)
  (when (not (jx/search-symbol classname))
    (beginning-of-line)
    (kill-whole-line)))

(defun jx/imported-classes ()
  "List all imported classes"
  (interactive)
  (let ((imported-classes nil))
     (while (re-search-forward "^import.*\\(\\.\\w+;\\)" nil t)
      (let ((found (match-string-no-properties 1)))
        (push (substring found 1 (- (string-width found) 1)) imported-classes)))
    imported-classes))


(defun jx/insert-imports (imports)
  "insert IMPORTS in buffer"
  (dolist (i (-sort 'string< imports))
    (insert i)
    (newline))
  (when imports
    (newline)))

(defun jx/filter-imports-by-group (imports group)
  "Sort IMPORTS by GROUP"
  (-filter (lambda (i)
             (and (string-match (format "^import %s\\..*;" group) i) i)) imports))

(defun jx/filter-static-imports (imports)
  "Sort static IMPORTS"
  (-filter (lambda (i)
             (and (string-match "^import static .*;$" i) i)) imports))

(defun jx/filter-other-imports (imports)
  (-remove (lambda (i)
             (and (string-match "^import \\(static\\)\\|\\(java\\)\\|\\(javax\\)\\|\\(org\\)\\|\\(com\\)\\..*;" i) i)) imports))


(defun jx/newline-if-necessary ()
  "Insert only one newline"
  (delete-blank-lines) ;;delete all unecessary blank lines
  (beginning-of-line)
  (when (not (looking-at "[ \t]*$"))
    (newline)))

(defun jx/sort-and-insert-imports (imports)
  "Sort and insert IMPORTS"
  (goto-line 3) ;; Imports should start at line 3
  (jx/insert-imports (jx/filter-static-imports imports))
  (dolist (group jx/group-import-order)
    (jx/insert-imports (jx/filter-imports-by-group imports group)))
  (jx/insert-imports (jx/filter-other-imports imports))
  (jx/newline-if-necessary))

(defun jx/sort-imports ()
  "Sort imports"
  (interactive)
  (goto-char (point-min))
  (let ((imports nil))
    ;; First remove all imports in buffer
    (while (re-search-forward "^import .*;$" nil t)
      (let ((found (match-string-no-properties 0)))
        (push found imports)
        (kill-whole-line)))
    ;; Second sort and insert imports
    (jx/sort-and-insert-imports imports)))

(defun jx/organize-imports ()
  "Organize imports"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((imported-classes (jx/imported-classes)))
      (while imported-classes
        (jx/clear-unused-imports (first imported-classes))
        (setq imported-classes (rest imported-classes))))
    (jx/sort-imports)))


(defvar javax-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(define-key java-mode-map (kbd "{") 'jx/electric-brace)
    (define-key java-mode-map (kbd "C-c C-b") 'jx/mvn-build)
    (define-key java-mode-map (kbd "C-c C-k") 'jx/mvn-compile)
    (define-key java-mode-map (kbd "C-c C-r") 'jx/mvn-test)
    (define-key java-mode-map (kbd "C-c C-t") 'jx/jump-between-tests-and-code)
    (define-key java-mode-map (kbd "C-c C-s") 'jx/src)
    (define-key java-mode-map (kbd "C-c C-o") 'jx/organize-imports)
    map)
  "Keymap for Javax mode.")


;;;###autoload
(define-minor-mode javax-mode
  "A minor mode to improve java experience."
  :lighter " jX"
  :keymap 'javax-mode-map)

;;;###autoload
(add-hook 'java-mode-hook 'javax-mode)

(provide 'javax-mode)
;;; javax.el ends here
