(require 'dashboard)

(dashboard-setup-startup-hook)

(setq dashboard-items '((projects . 5)
                        (bookmarks . 5)
                        (recents . 5)))

(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(provide 'dashboard_init)

;;; dashboard_init.el ends here
