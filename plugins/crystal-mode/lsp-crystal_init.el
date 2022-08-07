;; 注意，这里不要首先 require lsp-crystal, 否则将用默认的 scry 作为 server.
(setq lsp-clients-crystal-executable '("crystalline" "--stdio"))

;; (with-eval-after-load 'lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("crystalline"))
;;                     :activation-fn (lsp-activate-on "crystal")
;;                     :priority '1
;;                     :server-id 'crystalline)))

(require 'lsp-mode_init)

(defun zw/lsp-crystal-common-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (setq-local company-minimum-prefix-length 1)
  (lsp-deferred)
  )

(add-hook 'crystal-mode-hook 'zw/lsp-crystal-common-hooks)

(provide 'lsp-crystal_init)

;;; lsp-crystal_init.el ends here
