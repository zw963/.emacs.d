(require 'lsp-mode_init)
(require 'lsp-go)

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer t t)
            (add-hook 'before-save-hook #'lsp-organize-imports t t)
            (lsp-deferred)
            ))

(require 'dap-mode_init)
;; (require 'dap-go)
;; (dap-go-setup)
(require 'dap-dlv-go)
(setq dap-print-io t)

(provide 'lsp-go_init)

;;; lsp-go_init.el end
