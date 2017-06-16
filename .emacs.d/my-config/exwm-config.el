;; EXWM boot
(require 'exwm)
(require 'exwm-config)
(exwm-config-default)
;;(global-set-key (kbd "s-M-t") 'exwm-input-grab-keyboard)

;; keyboard shortcuts
(global-set-key (kbd "<XF86ScreenSaver>") (lambda ()
					    (interactive)
					    (start-process-shell-command
					     "slock" nil "slock")))
