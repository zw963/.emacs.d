(require 'valign)

(setq valign-fancy-bar t)

(add-hook 'gfm-mode-hook 'valign-mode)
(add-hook 'org-mode-hook 'valign-mode)

(provide 'valign_init)

;;; valign_init.el ends here
