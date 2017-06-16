;; android method have shell elsewhere...
(let* ((base-method (cdr (assoc "sshx" tramp-methods)))
       (new-method (copy-tree base-method))
       (rshell (assq 'tramp-remote-shell new-method)))
  (setcdr rshell "/system/bin/sh")
  (add-to-list 'tramp-methods (cons "android" new-method)))

;; accessing 'kodi' host with android method by default
(add-to-list
 'tramp-default-method-alist
 (list "\\`kodi\\'" nil "android"))
