(require 'lsp-mode_init)

(add-hook 'mint-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer t t)
            (setq-local company-minimum-prefix-length 1)
            (lsp-deferred)
            ))

(provide 'lsp-mint_init)

;;; lsp-mint_init.el ends here
