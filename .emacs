;; my emacs configuration


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(setq use-package-verbose t)

(setq emacs-config-path "/Devel/git/emacs-config/")
(setq custom-file (concat emacs-config-path ".gnu-emacs-custom"))
(load custom-file t t)

(load (concat emacs-config-path ".emacs-tcech.el"))
