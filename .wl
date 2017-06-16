;; mode:-*-emacs-lisp-*-
;; wanderlust
(setq
  elmo-maildir-folder-path "~/Mail"          ;; where i store my mail

  wl-stay-folder-window t                    ;; show the folder pane (left)
  wl-folder-window-width 30                  ;; toggle on/off with 'i'

  wl-smtp-posting-server "localhost"         ;; put the smtp server here
  wl-local-domain "suse.cz"                  ;; put something here...
  wl-message-id-domain "crashnator.suse.cz"  ;; ...

  wl-from "Tomas Cech <tcech@suse.com>"       ;; my From:

  ;; note: all below are dirs (Maildirs) under elmo-maildir-folder-path
  ;; the '.'-prefix is for marking them as maildirs
  wl-fcc ".Work.Sent"                       ;; sent msgs go to the "sent"-folder
  wl-fcc-force-as-read t                     ;; mark sent messages as read
  wl-default-folder ".Work"                  ;; my main inbox
  wl-draft-folder ".Work.Drafts"            ;; store drafts in 'postponed'
  wl-trash-folder ".Work.Trash"             ;; put trash in 'trash'
  wl-spam-folder ".Work.SPAM"              ;; ...spam as well
  wl-queue-folder ".queue"             ;; we don't use this

  ;; check this folder periodically, and update modeline
  wl-biff-check-folder-list '(".todo") ;; check every 180 seconds
                                       ;; (default: wl-biff-check-interval)

  ;; hide many fields from message buffers
  wl-message-ignored-field-list '("^.*:")
  wl-message-visible-field-list
  '("^\\(To\\|Cc\\):"
    "^Subject:"
    "^\\(From\\|Reply-To\\):"
    "^Organization:"
    "^Message-Id:"
    "^\\(Posted\\|Date\\):"
    )
  wl-message-sort-field-list
  '("^From"
    "^Organization:"
    "^X-Attribution:"
     "^Subject"
     "^Date"
     "^To"
     "^Cc")
  mime-view-buttons-visible t
;;  erc-hide-list '("JOIN" "PART" "QUIT")
  wl-draft-send-mail-function 'sendmail-send-it
  sendmail-program "/usr/bin/msmtp"
  message-sendmail-envelope-from 'header)

(setq wl-inotify-running nil)
(defun wl-inotify-callback (unused-param)
  (unless wl-inotify-running
    (setq wl-inotify-running t)
    (wl-folder-check-all)
    (setq wl-inotify-running nil)))

(or
 (inotify-add-watch "/home/tcech/Work_Mail/new" '(moved-from moved-to create) 'wl-inotify-callback)
 t)
(or
 (inotify-add-watch "/home/tcech/Work_Mail/L3.MyBugs/new" '(moved-from moved-to create) 'wl-inotify-callback)
 t)

(defun wl-mark-thread-as-read-and-next ()
  (interactive)
  (wl-thread-mark-as-read)
  (wl-summary-down))
(global-set-key (kbd "<f5>") 'wl-mark-thread-as-read-and-next)
(global-set-key (kbd "<f3>") 'wl-folder-check-all)

;; don't ****ing split large messages
(setq mime-edit-split-message nil)
