;(require 'auto-complete)

(defvar ac-source-rpm-source
  '((candidates . (directory-files (file-name-directory (or load-file-name buffer-file-name))))
    (prefix . "^\\(?:Source\\|Patch\\)[0-9]*:[[:blank:]]+\\(.*\\)")))

(defun ac-rpm-source-setup ()
  "Add the geiser completion source to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (add-to-list 'ac-sources
               'ac-source-rpm-source))

(setq ac-sources (list 'ac-source-rpm-source))

