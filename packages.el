(package! org-super-agenda)
(package! org-ql)
(package! org-edna)
(package! org-web-tools)
(package! dashboard)

;Org-Roam-Ui
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! orgmdb :recipe (:host github :repo "isamert/orgmdb.el"))
