;; Google Calendar
(use-package org-gcal :straight t :defer t
  :init
  (setq org-gcal-client-id (password-store-get "google/credentials/client-id")
	org-gcal-client-secret (password-store-get "google/credentials/client-secret")
	org-gcal-fetch-file-alist '(("tcech@purestorage.com" .  "~/org/pure-calendar.org")))
  (require 'org-gcal))

;; Google Mail
(use-package gnus :straight t :defer t
  :init
  (setq gnus-select-method
	'(nnimap "localhost"
		 (nnimap-inbox "INBOX")
		 (nnimap-split-methods default)
		 (nnimap-user "tcech@purestorage.com")
		 (nnimap-server-port 2993)
		 (nnimap-authenticator plain)
		 (nnimap-expunge t)
		 (nnimap-stream plain))))


;; JIRA
(use-package ejira
  :straight (ejira :type git :host github :repo "nyyManni/ejira")
  :init
  (setq jiralib2-url              "https://jira.purestorage.com"
        jiralib2-auth             'bearer
        jiralib2-token            (password-store-get "jira/token")

        ;; NOTE, this directory needs to be in `org-agenda-files'`
        ejira-org-directory       "~/jira"
        ejira-projects            '("CITI")

        ejira-priorities-alist    '(("Highest" . ?A)
                                    ("High"    . ?B)
                                    ("Medium"  . ?C)
                                    ("Low"     . ?D)
                                    ("Lowest"  . ?E))
        ejira-todo-states-alist   '(("To Do"       . 1)
                                    ("In Progress" . 2)
                                    ("Done"        . 3)))
  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  (require 'ejira-agenda)

  ;; Make the issues visisble in your agenda by adding `ejira-org-directory'
  ;; into your `org-agenda-files'.
  (add-to-list 'org-agenda-files ejira-org-directory)

  ;; Add an agenda view to browse the issues that
  (org-add-agenda-custom-command
   '("j" "My JIRA issues"
     ((ejira-jql "resolution = unresolved and assignee = currentUser()"
                 ((org-agenda-overriding-header "Assigned to me")))))))



;; Slack
(use-package tracking :straight t :defer t)
(use-package slack :straight t :defer t
  :init
  (add-hook 'slack-mode-hook #'emojify-mode)
  (if
      (auth-source-pick-first-password
       :host "purestorage.slack.com"
       :user "tcech@purestorage.com")
      (slack-register-team
       :name "purestorage"
       :token (auth-source-pick-first-password
               :host "purestorage.slack.com"
               :user "tcech@purestorage.com")
       :cookie (auth-source-pick-first-password
		:host "purestorage.slack.com"
		:user "tcech@purestorage.com^cookie")
       :subscribed-channels '(("citi-prg" "ir-citi-alerts"))))
  (with-eval-after-load 'tracking
    (define-key tracking-mode-map [f11]
      #'tracking-next-buffer))
  ;; Ensure the buffer exists when a message arrives on a
  ;; channel that wasn't open.
  (setq slack-buffer-create-on-notify t))


;; PostgreSQL
(setq sql-connection-alist
      '((assetsdb-readonly (sql-product 'postgres)
                    (sql-port 5432)
                    (sql-server "ci-metrics-db-read1.dev.purestorage.com")
                    (sql-user "readonly")
                    (sql-password "readonly")
                    (sql-database "triage_tool"))
	(assetsdb (sql-product 'postgres)
                    (sql-port 5432)
                    (sql-server "ci-metrics-db.dev.purestorage.com")
                    (sql-user "readonly")
                    (sql-password "readonly")
                    (sql-database "triage_tool"))))

(defun sql-connect-to-assetsdb-readonly ()
  (interactive)
  (sql-connect 'assetsdb-readonly "*pgsql-prod*"))

(defun sql-connect-to-assetsdb ()
  (interactive)
  (sql-connect 'assetsdb "*pgsql-prod*"))


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq company-tooltip-align-annotations t)

(use-package tide
  :straight t :defer t
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package web-mode
  :straight t :defer t
  :mode ("\\.tsx\\'" "\\.jsx\\'")
  )

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; (use-package typescript-mode
;;   :mode ("\\.ts\\'" "\\.tsx\\'")
;;   :straight t :defer t)

(use-package jest :straight t :defer t)

;; (use-package tree-sitter
;;   :ensure t
;;   :config
;;   ;; activate tree-sitter on any buffer containing code for which it has a parser available
;;   (global-tree-sitter-mode)
;;   ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
;;   ;; by switching on and off
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)
