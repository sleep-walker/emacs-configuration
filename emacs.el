(setq emacs-config-path "/home/tcech/git/emacs-configuration/")
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


;; UI changes
;; no startup message
(setq inhibit-startup-message t)
;; no menu bar
(menu-bar-mode -1)
;; no scrollbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; allow scalable fonts
(setq scalable-fonts-allowed t)

;; visual bell
(setq-default visible-bell t)

;; have ediff control window in same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;; emacs base packages
(use-package el-patch
  :straight t)

(use-package helm
  :straight t
  :init
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package cyberpunk-theme
  :straight t
  :init
  (load-theme 'cyberpunk))
(use-package leuven-theme :straight t)

;; orga stuff packages
(use-package org
  :defer t
  :straight t
  :init
  (setq org-hide-leading-stars 1)
  (setq org-cycle-separator-lines 3))

(use-package wanderlust
  :defer t
  :straight t
  :init
  (ignore-errors
    ;; org-wl is not available anymore, copy when handy, ignore otherwise
    (load-library "~/.emacs.d/org-wl.el")))


;; development packages
(use-package magit :straight t :defer t)
(use-package forge :straight t :defer t :after magit)
(use-package pyvenv :straight t :defer t)
(use-package treemacs :straight t :defer t)
(use-package eglot :straight t :defer t)
(use-package ag :straight t :defer t)
(use-package sane-term
  :defer t
  :straight t
  :bind (("C-x T" . sane-term-create)))


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

(use-package yaml-mode :straight t :defer t)
(use-package jinja2-mode :straight t :defer t)
