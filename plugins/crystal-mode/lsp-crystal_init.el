(require 'lsp-mode_init)

;; 注意，这里不要首先 require lsp-crystal, 否则将用默认的 scry 作为 server.
(setq lsp-clients-crystal-executable '("crystalline" "--stdio"))

(defun zw/lsp-crystal-common-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (lsp-deferred)
  )

(add-hook 'crystal-mode-hook 'zw/lsp-crystal-common-hooks)

(provide 'lsp-crystal_init)

;;; lsp-crystal_init.el ends here