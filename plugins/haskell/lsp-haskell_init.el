(require 'lsp-haskell)

(add-hook 'haskell-mode-hook #'lsp-deferred)
(add-hook 'haskell-literate-mode-hook #'lsp-deferred)

(provide 'lsp-haskell_init)

;;; lsp-haskell_init.el ends here
