(require 'lsp-mode_init)
;; (require 'dap-mode_init)
(require 'lsp-rust)

(setq lsp-rust-analyzer-display-chaining-hints t)
(setq lsp-rust-full-docs t)

;; (setq lsp-rust-server 'rls)

;; 注意，如果用 rls, 必须把 lsp-enable-imenu 设为 nil, ruby-analyzer 无需。
;; (setq-local lsp-enable-imenu nil)
(add-hook 'rust-mode-hook 'lsp-deferred)

(provide 'lsp-rust-mode_init)

;;; lsp-rust-mode_init.el ends here
