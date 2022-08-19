;; 注意，这里不要首先 require lsp-crystal, 否则将用默认的 scry 作为 server.
(require 'lsp-mode_init)

(defun zw/lsp-mint-common-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (setq-local company-minimum-prefix-length 1)
  (lsp-deferred)
  )

(add-hook 'mint-mode-hook 'zw/lsp-mint-common-hooks)

(provide 'lsp-mint_init)

;;; lsp-mint_init.el ends here
