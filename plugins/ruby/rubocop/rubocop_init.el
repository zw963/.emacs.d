(require 'rubocop)

(setq rubocop-format-on-save t)

(add-hook 'ruby-mode-hook #'rubocop-mode)

(provide 'rubocop_init)

;;; rubocop_init.el ends here
