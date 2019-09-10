;; my emacs configuration


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(setq use-package-verbose t)
;;(package-initialize)
(require 'use-package)

;; read all the secrets - from $HOME
(let ((file "~/.emacs-secrets.el.gpg"))
  (if (file-readable-p file)
      (load file)))

;;;;;
;; UI Basics
;;;;;;;;;;;;

;; Get rid of the startup message
(setq inhibit-startup-message t)

;; No menu bar
(menu-bar-mode -1)

;; No scroll bars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; allow scalable fonts
(setq scalable-fonts-allowed t)

;; visual bell
(setq-default visible-bell t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; make compatible with occurences used by ViM users
(define-coding-system-alias 'UTF-8 'utf-8)

;; Mouse related
(use-package mouse
  :init
  (xterm-mouse-mode t)
  (setq mouse-yank-at-point t))


;; Keys configuration
(global-set-key (kbd "S-<mouse-2>") 'clipboard-yank)
;;(global-set-key (kbd "S-<mouse-2>") 'mouse-yank-primary)
(global-set-key (kbd "C-x p") (lambda () (interactive (other-window -1))))


(defun on-guix? ()
  (file-exists-p "/run/current-system"))


(if
    (ignore-errors (set-face-attribute 'default nil :font "xos4 Terminus" :height 100)
		   t)
    t
  (set-face-attribute 'default nil :font "Terminus" :height 100))

;; Shows parenthesis
(show-paren-mode 1)
;; Shows column number
(column-number-mode 1)

;; Save history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))


;;;;;
;; All programming modes
;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook
	  (lambda ()
            ;; show function name where is cursor
            (which-function-mode 1)
            ;; show trailing whitespace
	    (setq-default show-trailing-whitespace t)
	    (if (fboundp 'compile)
		(define-key (current-global-map) (kbd "C-c C-l") 'compile))
	    (if (fboundp 'recompile)
		(define-key (current-global-map) (kbd "C-c C-S-l") 'recompile))))


(defun highlight-fixme-todo-bug ()
  "Higlight FIXME:, TODO: and BUG: so they're easily visible in the code"
	    (font-lock-add-keywords
	     nil
	     '(("\\b\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'highlight-fixme-todo-bug)

;;;;;
;; Programming modes
;;;;;;;;;;;;;;;;;;;;

;; Python related
(add-hook 'inferior-python-mode-hook
 	  (lambda ()
	    (setenv "PAGER" "cat")
            (setq show-trailing-whitespace nil)
	    (xtags-mode)))


;; C/C++
(add-hook 'c-mode-hook 'xcscope-mode)

;; Shell
;; no here document (EOF block) in shell script mode
(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))

;;;;;
;; Various modes
;;;;;;;;;;;;;;;;


;; eshell - more natural completion
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)
   (setq show-trailing-whitespace nil)))

(defun term-send-shift-right () (interactive) (term-send-raw-string "\e[1;2C"))
(defun term-send-shift-left  () (interactive) (term-send-raw-string "\e[1;2D"))

(add-hook
 'term-mode-hook
 (lambda ()
   (setq show-trailing-whitespace nil)
   (bind-key "<C-S-c>" 'kill-ring-save)
   (bind-key "<S-right>" 'term-send-shift-right)
   (bind-key "<S-left>" 'term-send-shift-left)))

;; tramp
;;(load "~/.emacs.d/my-config/tramp-config.el")

;; let commit message look less weird (it's not intended for diff mode)
(add-to-list 'auto-mode-alist '("osc-editor.*\.diff" . text-mode))


(add-hook 'c-mode-common-hook
          (lambda ()
	    (define-key c-mode-base-map (kbd "C-c C-l") 'compile)
	    (define-key c-mode-base-map (kbd "C-c C-S-l") 'recompile)))

;; FIXME: why was this here?
;; (delete 'Git vc-handled-backends)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . diff-mode))

(use-package rst-mode
  :mode "\\.rst\\'"
;; :interpreter rst-mode
  :load-path (lambda () (concat emacs-config-path ".emacs.d/unsorted")))

(use-package markdown-mode
  :mode "\\.\\(text\\|mdwn\\|pmdwn\\)"
  :load-path (lambda () (concat emacs-config-path ".emacs.d/unsorted")))

(autoload 'xgtags-mode (concat emacs-config-path "/.emacs.d/unsorted/xgtags.el") "GTags with XCScope-like controls" t)


;; Bugz mode
(use-package bugz-mode
  :commands (bugz-mode bugz)
  :load-path "/Devel/git/bugz-mode/bugz-mode.el"
  :init
  (setq bugz-db-base "")
  (setq bugz-db-user "")
  (setq bugz-dont-ask-for-password 1))

;; RPM stuff
(use-package rpm-spec-mode
  :load-path (lambda () (concat emacs-config-path ".emacs.d/unsorted"))
  :mode ("\\.spec" . rpm-spec-mode)
  :init
  (setq compile-command
	"osc build --ccache --linksources --no-service openSUSE_Tumbleweed x86_64")
  (setq-default indent-tabs-mode nil)
  ;; init rpm autocomplete
  (load-library (concat emacs-config-path ".emacs.d/my-config/ac-rpm.el"))
  (auto-complete-mode)
  (highlight-fixme-todo-bug))

;; Tramp
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")


(use-package org
	     :defer t
	     :init
	     (setq org-hide-leading-stars 1)
             (setq org-cycle-separator-lines 3))


(use-package helm
  :init
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))


(use-package magit
  :defer t)

(use-package forge
  :after magit)

(use-package pyvenv
  :defer t)

(use-package sane-term
  :bind (("C-x T" . sane-term-create)))

;; Terminal buffer configuration.
(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20611
  (setq bidi-paragraph-direction 'left-to-right))


(use-package wl
  :defer t
  :init
  (load-library "~/.emacs.d/org-wl.el"))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         30)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package gitignore-mode
  :defer t)

;; Browser
(setq browse-url-generic-program "/home/tcech/bin/url-handler.sh")
(setq browse-url-browser-function 'browse-url-generic)

(use-package sql
  :defer t
  :config
  (progn
    (sql-set-product-feature 'mysql :prompt-regexp "^\\([a-zA-Z0-9().-]*\\) \\[[_a-zA-Z0-9().-]*\\]> ")
    (toggle-truncate-lines)))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))


;; IRC
(defun connect-sleep-server ()
  (interactive)
  (require 'gnutls)
  (add-to-list 'gnutls-trustfiles (expand-file-name "~/ssl/sleep-server-weechat-relay.pem"))
  (weechat-connect "sleep-walker.cz" 9001 sleep-server-relay-pass 'ssl nil)
  (weechat-monitor-all-buffers)
  ;; Sauron will show in the same window
  (setq sauron-separate-frame nil)
  (sauron-start)
  (load-library
   (car
    (file-expand-wildcards "/home/tcech/.emacs.d/elpa/weechat*/weechat-sauron.el"))))

(defun connect-suse-irc ()
  (interactive)
  (weechat-connect "localhost" 9000 crashnator-relay-pass 'plain nil)
  (weechat-monitor-all-buffers))


;; SSH agent
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun read-agent-env ()
  "Read environment variables related to SSH agent."
  (interactive)
  (let* ((s (get-string-from-file "~/.ssh/agent"))
        (pid (progn
              (string-match "SSH_AGENT_PID=\\([0-9]+\\)[^0-9].*" s)
              (match-string 1 s)))
        (socket (progn
                 (string-match "SSH_AUTH_SOCK=\\([^\n;]+\\)\\(;\\|$\\)" s)
                 (match-string 1 s))))
    (setenv "SSH_AGENT_PID" pid)
    (setenv "SSH_AUTH_SOCK" socket)
    (message "pid: %s\nsocket: %s" pid socket)))


(read-agent-env)

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

;; defunct yet
(defun pandoc-markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Pandoc Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"https://raw.githubusercontent.com/jakov/js-pandoc/master/js-pandoc.js\"></script><script>var html = text.Pandoc();</script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

;;(setq load-home-init-file t) ; don't load init file from ~/.xemacs/init.el

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; (setq jenkins-api-token "d818a163901a6bdbd2f3a23f129d4426")
;; (setq jenkins-url "https://ci.intgdc.com/api/xml")
;; (setq jenkins-username "tomas.cech")

(defun org-export-nakup ()
  (interactive)
  (let* ((e (org-element-at-point))
         (begin (org-element-property :contents-begin e))
         (end   (org-element-property :contents-end e)))
    (shell-command-on-region
     begin end
     "sed -n '/[^[:blank:]]/s@^@[ ] @p' > ~/tmp/nakup.txt"
     nil nil nil nil nil)))

(defun user-mail-address ()
  "tomas.cech@ubnt.com")

;; (defun prepare-editor-for-term ()
;;   (unless (boundp 'ansi-term-session-name)
;;     (let ((srvname (read-string "server name: ")))
;;       (setq ansi-term-session-name srvname)
;;       (setq server-name srvname)
;;       (server-start)
;;       (setenv "EMACS_SESSION" srvname))))

;; (add-hook 'shell-mode-hook  'with-editor-export-editor)
;; (add-hook 'term-exec-hook   'with-editor-export-editor)
;; (add-hook 'eshell-mode-hook 'with-editor-export-editor)

(setq geiser-active-implementations '(guile))

(defun comint-clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(add-hook 'inferior-python-mode-hook (lambda () (local-set-key (kbd "C-c l") 'comint-clear))) 

(setenv "PAGER" "cat")

(defun start-server-with-name ()
  (interactive)
  (setq server-name (read-string "Server name: "))
  (server-start))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; (use-package tide
;;   :defer t
;;   :init
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (setq company-tooltip-align-annotations t)
;;   (add-hook 'before-save-hook 'tide-format-before-save))

;;  (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; formats the buffer before saving


