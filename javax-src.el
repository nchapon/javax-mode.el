;; (defvar jx/src-libs-dir ".javax-sources")

;; (defun jx/dirname (lib)
;;   "Returns dirname for LIB"
;;   (string-match ".*/\\(.*\\).jar$" lib)
;;   (match-string 1 lib))

;; (defun jx/src-archive (lib)
;;   "Archive source name of LIB"
;;   (format "%s-sources.jar" (file-name-sans-extension (jx/expand-m2-repo-path lib))))


;; (defun jx/extract-archive  (lib)
;;   "DOCSTRING"
;;   (let* ((src (jx/src-archive lib))
;;         (lib-dir (jx/dirname lib))
;;         (dir (format "%s.javax-sources" (jx/project-dir))))
;;     (and
;;      (f-exists? src)
;;      (not (f-exists? (format "%s/%s" dir lib-dir)))
;;      (progn
;;        (f-mkdir dir lib-dir)
;;        (shell-command-to-string (format "unzip -d %s.javax-sources/%s %s" (jx/project-dir) lib-dir src))))))

;; Perhaps the best is to keep source in mvn repository and index them with gtags
(defun jx/download-sources ()
 "Get project dependencies source code and store them in .javax-sources"
 (interactive)
 (let ((output-dir (format "%s/.javax-sources" (f-long (jx/project-dir)))))
   (progn
     (f-mkdir output-dir)
     (jx/mvn (format "dependency:unpack-dependencies -Dclassifier=sources -Dmdep.useSubDirectoryPerArtifact=true -DoutputDirectory=%s -DfailOnMissingClassifierArtifact=true" output-dir)))))


(provide 'javax-src)
