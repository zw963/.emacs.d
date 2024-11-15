(require 'valign)

(setq valign-fancy-bar t)

;; (setq valign-max-table-size 10000)

(add-hook 'gfm-mode-hook 'valign-mode)
(add-hook 'org-mode-hook 'valign-mode)

(provide 'valign_init)

;;; valign_init.el ends here
