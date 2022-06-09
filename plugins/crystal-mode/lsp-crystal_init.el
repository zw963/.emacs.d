;; 注意，这里不要首先 require lsp-crystal, 否则将用默认的 scry 作为 server.
(setq lsp-clients-crystal-executable '("crystalline" "--stdio"))

(require 'lsp-mode_init)

(defun zw/lsp-crystal-common-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (setq-local company-minimum-prefix-length 1)
  (lsp-deferred)
  )

(add-hook 'crystal-mode-hook 'zw/lsp-crystal-common-hooks)

(provide 'lsp-crystal_init)

;;; lsp-crystal_init.el ends here
