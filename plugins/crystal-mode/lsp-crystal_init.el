(require 'lsp-mode_init)
(require 'lsp-crystal)

(defun zw/lsp-crystal-common-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (lsp-deferred)
  )

(add-hook 'crystal-mode-hook 'zw/lsp-crystal-common-hooks)

(provide 'lsp-crystal_init)

;;; lsp-crystal_init.el ends here
