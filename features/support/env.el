(require 'f)

(defvar javax-mode-support-path
  (f-dirname load-file-name))

(defvar javax-mode-features-path
  (f-parent javax-mode-support-path))

(defvar javax-mode-root-path
  (f-parent javax-mode-features-path))

(add-to-list 'load-path javax-mode-root-path)



(require 'javax-mode)
(require 'espuds)
(require 'ert)



(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(defun save-all-buffers-dont-ask ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (let ((filename (buffer-file-name)))
        (when (and filename
                   (or (file-exists-p filename)
                       (s-ends-with? ".java" filename)))
          (save-buffer))))))

(defun kill-matching-buffers-dont-ask (regexp &optional internal-too)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))



(After
 (save-all-buffers-dont-ask)
 (kill-matching-buffers-dont-ask "java")
 (delete-directory (expand-file-name "tmp" javax-mode-root-path) t))



(Teardown
 ;; After when everything has been run
 )
