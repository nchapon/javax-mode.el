(defun jx/dirname (lib)
  "Returns dirname for LIB"
  (string-match ".*/\\(.*\\).jar$" lib)
  (match-string 1 lib))

(defun jx/src-archive (lib)
  "Archive source name of LIB"
  (format "%s-sources.jar" (file-name-sans-extension (jx/expand-m2-repo-path lib))))

(defun jx/extract-archive  (lib)
  "DOCSTRING"
  (let* ((src (jx/src-archive lib))
        (lib-dir (jx/dirname lib))
        (dir (format "%s.javax-sources/%s" (jx/project-dir) lib-dir)))
    (and
     (f-exists? src)
     (not (f-exists? dir))
     (progn
       (f-mkdir dir)
       (shell-command-to-string (format "unzip -d %s.javax-sources/%s %s" (jx/project-dir) lib-dir src))))))

(defun jx/src-libs ()
 "Get project dependencies source code and store them in .javax-sources"
 (interactive)
 (-map
  'jx/extract-archive
  (cdr (assoc :classpath (jx/read-config-file)))))


(provide 'javax-src)
;;; javax-src ends here


;; (cond
;;  (t (shell-command-to-string
;;   (format "find %s -iname %s.java -print0 | grep -FzZ %s"
;;           (jx/project-dir) "Assert" "org/junit")))
;;  (t (shell-command-to-string
;;   (format "find %s%s -iname %s.java -print0 | grep -FzZ %s"
;;           (jx/project-dir) jx/src-libs-dir "Assert" "org/junit"))))
