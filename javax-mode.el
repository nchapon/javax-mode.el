;;; javax-mode.el --- Minor mode to improve java experience in Emacs
(require 'cc-mode)
(require 'compile)



(defcustom javax-mvn-build-command "mvn -f %spom.xml clean install"
  "Format string to run mvn build command. his format string
   should use '%s' to substitute the project maven root
   directory."
  :group 'javax
  :type 'string
  :safe 'stringp)

(defcustom javax-mvn-compile-command "cd %s; mvn -o compile"
  "Format string to run mvn compile command. This format string
   should use '%s' to substitute the project maven root
   directory."
  :group 'javax
  :type 'string
  :safe 'stringp)







(defun javax-mvn-build ()
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
                                     (format javax-mvn-build-command dir) nil nil 'compile-history)))))

(defun javax-mvn-compile ()
  "Runs mvn test from a buffer file"
  (interactive)
  (compile
        (format
         javax-mvn-compile-command
         (locate-dominating-file (buffer-file-name) "pom.xml")
         (car(split-string (buffer-name) "\\.")))))

(defun javax-mvn-test ()
  "Runs mvn test from a buffer file"
  (interactive)
  (compile
        (format
         "cd %s; mvn -Dtest=%s test"
         (locate-dominating-file (buffer-file-name) "pom.xml")
         (car(split-string (buffer-name) "\\.")))))

;;; For maven 2/3 output
(add-to-list 'compilation-error-regexp-alist
             '("^.*?\\(/.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3))


(defun javax-path-for (package)
  "Convert PACKAGE name to real path."
  (interactive)
  (let ((segments (split-string package "\\." t)))
    (mapconcat 'identity segments "/")))


(defun javax-find-package ()
  "Find current buffer package"
  (interactive)
  (save-excursion
    (goto-char (point-min))
       (when (re-search-forward "\\(^package \\(.*\\);$\\)" nil t))
          (match-string-no-properties 2)))


(defun javax-find-symbol-package (symbol)
  "Find the package for the current SYMBOL"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (format "\\(^import \\(.*\\)%s;$\\)" symbol) nil t)
        (match-string-no-properties 2)
      (javax-find-package)))))

(defun javax-electric-brace ()
  "Insert automatically close brace after 2 new lines."
  (interactive)
  (insert " {")
  (backward-char)
  (fixup-whitespace)
  (move-end-of-line 1)
  (indent-for-tab-command)
  (insert "\n\n")
  (insert "}")
  (indent-for-tab-command)
  (previous-line)
  (indent-for-tab-command))

(defun javax-symbol-at-point ()
  "Read symbol at point"
  (interactive)
  (let ((str (thing-at-point 'symbol)))
    str))

(defun javax-read-symbol-name (prompt callback &optional query)
  "Read symbol name."
  (let ((symbol-name (javax-symbol-at-point)))
    (cond
     ((not (or current-prefix-arg query (not symbol-name)))
      (funcall callback symbol-name))
     (t (funcall callback (read-from-minibuffer prompt))))))

(defun javax-project-dir ()
  "Returns java project dir for current buffer"
  (locate-dominating-file (buffer-file-name) ".git"))

(defun javax-find-file (symbol)
  "Find java file from project root"
  (let ((package (javax-find-symbol-package symbol)))
    (shell-command-to-string
    (format "find %s -iname %s.java -print0 | grep -FzZ %s"
            (javax-project-dir) symbol (javax-path-for package)))))

(defun javax-src-handler (symbol)
  "Create a handler to lookup java source code for SYMBOL"
  (let ((results (javax-find-file symbol)))
    (cond
     ((string= "" results) (message "No source file for symbol %s" symbol))
     (t (find-file (first (split-string results)))))))

(defun javax-src (query)
  "Open java source file for the given QUERY.
Defaults to the symbol at point. With prefix arg or no symbol under
point, prompts for a var"
  (interactive "P")
  (javax-read-symbol-name "Class :" 'javax-src-handler query))

(defun javax-in-tests-p ()
  "Check whether the current file is a test file."
  (string-match-p "src/test/java" (buffer-file-name)))

(defun javax-test-for (package)
  "Returns the path of the the test file for a given PACKAGE."
  (format
   "%ssrc/test/java/%s/%sTest.java"
   (locate-dominating-file (buffer-file-name) "pom.xml")
   (javax-path-for package)
   (car (split-string (buffer-name) "\\.java"))))

(defun javax-implementation-for (package)
  "Returns the path of the the implementaion file for a given PACKAGE."
  (format
   "%ssrc/main/java/%s/%s.java"
   (locate-dominating-file (buffer-file-name) "pom.xml")
   (javax-path-for package)
   (car (split-string (buffer-name) "Test\\.java"))))

(defun javax-jump-to-test ()
  "Jump from implementation file to test."
  (interactive)
    (find-file (javax-test-for (javax-find-package))))

(defun javax-jump-to-implementation ()
  "Jump from test file to implementation."
  (find-file (javax-implementation-for (javax-find-package))))

(defun javax-jump-between-tests-and-code ()
  "Jump between tests and code"
  (interactive)
  (if (javax-in-tests-p)
       (javax-jump-to-implementation)
    (javax-jump-to-test)))

(defun javax-search-symbol (symbol)
  "Search symbol, should be case sensitive."
  (let ((case-fold-search nil))
    (re-search-forward (format "\\b%s\\b" symbol) nil t)))

(defun javax-clear-unused-imports (classname)
  "Clear unused imported classes."
  (goto-char (point-min))
  (javax-search-symbol classname)
  (let ((kill-whole-line t))
    (when (not (javax-search-symbol classname))
      (beginning-of-line)
      (kill-line))))

(defun javax-imported-classes ()
  "List all imported classes"
  (interactive)
  (let ((imported-classes nil))
     (while (re-search-forward "^import.*\\(\\.\\w+;\\)" nil t)
      (let ((found (match-string-no-properties 1)))
        (push (substring found 1 (- (string-width found) 1)) imported-classes)))
    imported-classes))


(defun insert-import (import)
  "insert IMPORT in buffer"
  (insert import)
  (newline))

(defun javax-sort-imports ()
  "Sort imports"
  (interactive)
  (goto-char (point-min))
  (let ((imports nil))
     (while (re-search-forward "^import.*\\(\\.\\w+;\\)" nil t)
       (let ((found (match-string-no-properties 0)))
        (push found imports)
        (delete-region (point-at-bol) (point-at-eol))))
     (goto-line 3) ;;Imports should start at line 3
     (mapcar 'insert-import (sort imports 'string<))))

(defun javax-organize-imports ()
  "Organize imports"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((imported-classes (javax-imported-classes)))
      (while imported-classes
        (javax-clear-unused-imports (first imported-classes))
        (setq imported-classes (rest imported-classes))))
    (javax-sort-imports)))



(defvar javax-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key java-mode-map (kbd "{") 'javax-electric-brace)
    (define-key java-mode-map (kbd "C-c C-b") 'javax-mvn-build)
    (define-key java-mode-map (kbd "C-c C-k") 'javax-mvn-compile)
    (define-key java-mode-map (kbd "C-c C-r") 'javax-mvn-test)
    (define-key java-mode-map (kbd "C-c C-t") 'javax-jump-between-tests-and-code)
    (define-key java-mode-map (kbd "C-c C-s") 'javax-src)
    (define-key java-mode-map (kbd "C-c C-o") 'javax-organize-imports)
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
