(require 'lsp-haskell)

(add-hook 'haskell-mode-hook #'lsp-mode-common-hooks)
(add-hook 'haskell-literate-mode-hook #'lsp-mode-common-hooks)

(provide 'lsp-haskell_init)

;;; lsp-haskell_init.el ends here
