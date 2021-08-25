(require 'dart-mode)

(setq dart-format-on-save t)

(add-hook 'dart-mode-hook 'lsp)

(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

(provide 'dart-mode_init)

;;; dart-mode_init.el ends here
