(load-file "~/.doom.d/private.el")
(setq doom-theme 'doom-tomorrow-night)

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
        org-agenda-skip-scheduled-if-done t
        org-log-done t
        org-log-into-drawer t
        org-todo-keywords
                '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
        org-capture-templates
        `(("i" "Inbox" entry  (file "inbox.org") ,(concat "* TODO %?\n" "/Entered on/ %U")))

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

(use-package org-pretty-tags
  :diminish org-pretty-tags-mode
  :ensure t
  :config
  (setq org-pretty-tags-surrogate-strings
        '(("work"  . "⚒")))
  (org-pretty-tags-global-mode))
