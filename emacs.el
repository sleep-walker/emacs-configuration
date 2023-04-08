(setq emacs-config-path (expand-file-name "~/git/emacs-configuration/"))
(setq custom-file (concat emacs-config-path "gnu-emacs-custom"))
(load custom-file t t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'el-patch)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Basics
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; make compatible with occurences used by ViM users
(define-coding-system-alias 'UTF-8 'utf-8)
;; don't ask when editing symlink - edit symlink target
(setq vc-follow-symlinks t)

;; UI changes
;; no startup message
(setq inhibit-startup-message t)
;; no menu bar
(menu-bar-mode -1)
;; no scrollbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; set thresholds not to split anymore
(setq split-width-threshold 120)
(setq split-height-threshold 120)

;; allow scalable fonts
(setq scalable-fonts-allowed t)

;; visual bell
(setq-default visible-bell t)

;; have ediff control window in same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(ignore-errors (set-face-attribute 'default nil :font "xos4 Terminus" :height 130) t)
(ignore-errors (set-face-attribute 'default nil :font "Terminus"      :height 130) t)

;; emacs base packages
(use-package el-patch
  :straight t)

(use-package helm
  :straight t
  :init
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package helm-themes :straight t :defer t)
(use-package cyberpunk-theme :straight t :defer t :init (load-theme 'cyberpunk))
(use-package leuven-theme :straight t :defer t)

;; orga stuff packages
(use-package org
  :defer t
  :straight t
  :init
  (setq org-hide-leading-stars 1)
  (setq org-cycle-separator-lines 3)
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture))
(use-package ob-sql-mode)
(use-package dash :straight t :defer t)
(use-package password-store :straight t :defer t)
(use-package real-auto-save :straight t :defer t)

(use-package vterm :straight t :defer t
  :init
  (setq vterm-max-scrollback 10000))

(use-package multi-vterm :straight t :defer t
  :init
  (global-set-key (kbd "C-x T") 'multi-vterm))

;; development packages
(use-package magit :straight t :defer t)
(use-package forge :straight t :defer t :after magit)
(use-package treemacs :straight t :defer t)
(use-package eglot :straight t :defer t)
(add-hook 'python-mode-hook 'eglot-ensure)

(use-package ag :straight t :defer t)
(use-package wgrep-ag :straight t :defer t)

(use-package cram-mode
  :straight (cram-mode :type git :host github :repo "signalpillar/cram-mode")
  :defer t
  :init
  (require 'cram-mode))

(use-package python-coverage
  :straight (python-coverage :type git :host github :repo "wbolster/emacs-python-coverage")
  :defer t)


(use-package projectile
  :straight t
  :defer t
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Programming language modes
(use-package groovy-mode :straight t :defer t)
(use-package yaml-mode :straight t :defer t)
(use-package jinja2-mode :straight t :defer t)
(use-package dockerfile-mode :straight t :defer t)
(use-package pyvenv :straight t :defer t)
(use-package flycheck :straight t :defer t)


(setq python-python-command "ipython3")
(use-package python-black :straight t :defer t)

(use-package go-mode
  :mode "\\.go\\'"
  :straight t
  :defer t)
(use-package company :straight t :defer t)
(use-package flycheck :straight t :defer nil)
(use-package flycheck-golangci-lint :straight t :defer t :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package rg :straight t :defer t)

;; configure jsx-tide checker to run after your default jsx checker
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)


;; useful for slack and other :)
(use-package emojify :straight t :defer t)

(use-package tsi
  :after tree-sitter
  :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; read hostname specific configuration, allow override of common configuration
(let ((hostname-file
       (concat emacs-config-path "emacs-" (system-name) ".el")))
      (if (file-exists-p hostname-file)
         (load hostname-file nil 'nomessage)
       t))
