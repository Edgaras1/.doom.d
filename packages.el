(package! org-super-agenda)
(package! org-ql)
(package! modus-themes)
(package! moe-theme)
(package! org-edna)
(package! yequake)
(package! org-web-tools)
(package! dashboard)

;Org-Roam-Ui
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
