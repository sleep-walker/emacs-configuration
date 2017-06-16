(defun insert-zero-width-space ()
  (interactive)
  (insert-char #x200b))

(define-key org-mode-map (kbd "C-*") 'insert-zero-width-space)

(setq org-emphasis-regexp-components
      '("   ('\"{\x200B" "-     .,:!?;'\")}\\[\x200B" "     
,\"'" "." 1))



