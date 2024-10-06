(require 'lsp-mode_init)
(setq lsp-elixir-ls-version "v0.23.0")
(require 'lsp-elixir)

(add-hook 'elixir-ts-mode-hook 'lsp-mode-common-hooks)

(provide 'lsp-elixir_init)

;;; lsp-elixir_init.el ends here
