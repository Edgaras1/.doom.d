(setq doom-theme 'doom-one)
;;Directories
(setq
      org-directory "~/Dropbox/ORG"
      org-agenda-files '("~/Dropbox/ORG") )

;;(menu-bar-mode t) ;;-1/t enable/disable top menu
;;(define-key global-map [menu-bar words]
;;  (cons "Words" (make-sparse-keymap "Words")))

;; Define specific subcommands in this menu.
;;(define-key global-map
;;  [menu-bar words forward]
;;  '("Forward word" . forward-word))

;;(define-key global-map
;;  [menu-bar words backward]
;;  '("Backward word" . backward-word))

;;Surpress/hide/remove existing menu item. this example removes 'Edit'
;;(define-key global-map [menu-bar edit] 'undefined)

(tool-bar-mode t)

(defun org-agenda-show-custom (&optional arg)
  (interactive "P")
  (org-agenda arg "g"))

(tool-bar-add-item "spell" 'org-agenda-show-custom
                'Agenda
                :help    "Show agenda")

(tool-bar-add-item "spell" 'org-toggle-narrow-to-subtree
                'un-narrow
                :help    "(Un)Narrow to subtree")


(tool-bar-add-item "spell" 'org-pomodoro
                'Pomodoro
                :help    "Start stop pomodoro")

(setq tool-bar-button-margin 0) ;;set margin of toolbar

(defun tangle-and-reload ()
  "Run `org-babel-tangle` and `doom/reload' in sequence."
  (interactive)
  (org-babel-tangle)
  (doom/reload))

(global-set-key (kbd "C-c t r") 'tangle-and-reload)

(after! org
  (define-key global-map (kbd "C-c c") 'org-capture)

  (defun log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)


  (defun org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i"))
  (define-key global-map (kbd "C-c i") 'org-capture-inbox)

  (setq
        org-hide-block-startup t
        org-agenda-skip-scheduled-if-done t
        org-log-done t
        org-log-into-drawer t
        org-todo-keywords
                '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))

        org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)"))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil))

(use-package! org-super-agenda
  :after org-agenda
  :config

  (setq org-agenda-hide-tags-regexp "."
        org-agenda-block-separator nil)

  (setq org-agenda-custom-commands
        '(("g" "My agenda"(
                           (agenda "" (
                                       (org-agenda-start-day "1d")
                                       (org-agenda-start-on-weekday nil)
                                       (org-agenda-span 1)                      ;; S%2d:
                                       (org-agenda-scheduled-leaders (quote ("" ""))) ;; replace 'Scheduled:'
                                       (org-super-agenda-groups'(

                                                                 (:name none :deadline t :order 0)
                                                                 (:name none :time-grid t :order 1)

                                                                 (:name "Daily medicine" :and (:scheduled t :tag "medicine") :order 2)
                                                                 (:name "Other" :order 3 :scheduled t )
                                                                 (:discard (:anything))))
                                       ))
                           (todo "NEXT"
                                 ((org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'deadline))
                                  (org-agenda-prefix-format "  %i %-12:c [%e] ")
                                  (org-agenda-overriding-header "\nTasks\n")))
                           (tags-todo "inbox"
                                      ((org-agenda-prefix-format "  %?-12t% s")
                                       (org-agenda-overriding-header "\nInbox\n")))
                           (tags "CLOSED>=\"<today>\""
                                 ((org-agenda-overriding-header "\nCompleted today\n")))))))(org-super-agenda-mode))

(defun abs--quick-capture ()
       ;; redefine the function that splits the frame upon org-capture
       (defun abs--org-capture-place-template-dont-delete-windows (oldfun args)
         (cl-letf (((symbol-function 'org-switch-to-buffer-other-window) 'switch-to-buffer))
           (apply oldfun args)))

       ;; run-once hook to close window after capture
       (defun abs--delete-frame-after-capture ()
         (delete-frame)
         (remove-hook 'org-capture-after-finalize-hook 'abs--delete-frame-after-capture)
         )

       ;; set frame title
       (set-frame-name "emacs org capture")
       (add-hook 'org-capture-after-finalize-hook 'abs--delete-frame-after-capture)
       (abs--org-capture-place-template-dont-delete-windows 'org-capture nil)
  )

(use-package yequake
  :custom
  (yequake-frames
   '(("org-capture"
      (buffer-fns . (yequake-org-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t)))))))

  (use-package org-pomodoro
    :config
    (setq org-pomodoro-length 25
          org-pomodoro-short-break-length 5
          org-pomodoro-long-break-length 30
          org-pomodoro-manual-break t
          org-pomodoro-clock-break nil
          org-pomodoro-play-sounds nil
          org-pomodoro-keep-killed-pomodoro-time t)

    (defun ed/toggle-music(action)
        (let ((command (concat "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." action)))
        (shell-command command)))

    (add-hook 'org-pomodoro-tick-hook
          '(lambda ()
            (shell-command (format "awesome-client 'screen[1].pomodoro:set_markup(\"%s\")'" (my/org-pomodoro-time)))))


    (add-hook 'org-pomodoro-started-hook
	      (apply-partially #'ed/toggle-music "Play"))

    (add-hook 'org-pomodoro-killed-hook
          '(lambda ()
            (ed/toggle-music "Pause")
            (shell-command (format "awesome-client 'screen[1].pomodoro:set_markup(\" <u>No Active task</u>\")'"))
            ))

    (add-hook 'org-pomodoro-overtime-hook
	      (apply-partially #'ed/toggle-music "Pause"))

    (add-hook 'org-pomodoro-finished-hook
	      (apply-partially #'ed/toggle-music "Pause"))
    )


 (defun my/org-pomodoro-time ()
  "Return the remaining pomodoro time"
  (if (org-pomodoro-active-p)
      (cl-case org-pomodoro-state
        (:pomodoro      (format " %d: %s - %s" org-pomodoro-count (org-pomodoro-format-seconds) (substring-no-properties org-clock-heading)))
        (:short-break   (format " Short break time: %s" (org-pomodoro-format-seconds)))
        (:long-break    (format " Long break time: %s" (org-pomodoro-format-seconds)))
        (:overtime      (format " <u>Overtime!</u> %s" (org-pomodoro-format-seconds))))
                        " <u>No Active task</u>"))

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/Dropbox/ORG/Contacts.org")))


(use-package org-capture
  :ensure nil
  :after org
  :preface
  (defvar my/org-contacts-template "* %(org-contacts-template-name)
        :PROPERTIES:
        :ADDRESS: %^{Street, City, Country}
        :BIRTHDAY: %^{yyyy-mm-dd}
        :EMAIL: %(org-contacts-template-email)
        :NOTE: %^{NOTE}
        :END:" "Template for org-contacts.")

  :custom
  (org-capture-templates
   `(("c" "Contact" entry (file "~/Dropbox/ORG/Contact.org"), my/org-contacts-template :empty-lines 1)
     ("i" "Inbox" entry  (file "~/Dropbox/ORG/inbox.org") ,(concat "* TODO %?\n" "/Entered on/ %U")))


                ))

;autosave org-buffers
(defun my-org-mode-autosave-settings ()
        (add-hook 'auto-save-hook 'org-save-all-org-buffers nil nil))
        (add-hook 'org-mode-hook 'my-org-mode-autosave-settings)




;hide markers
(setq org-hide-emphasis-markers t)
