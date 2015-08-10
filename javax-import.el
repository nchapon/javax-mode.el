(defun jx/add-import-if-necessary (import)
  "Add IMPORT if necessary"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (not (re-search-forward (format "^import %s;" import) nil t))
          (progn
            (goto-char (point-min))
            (forward-line 1)
            (insert "\n")
            (insert (format "import %s;\n" import)))))))

(provide 'javax-import)
