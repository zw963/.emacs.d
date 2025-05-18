;; -*- lexical-binding: t; -*-

;; 注意，这里不要首先 require lsp-crystal, 否则将用默认的 scry 作为 server.
;; (setq lsp-clients-crystal-executable '("crystalline" "--stdio"))

;; (with-eval-after-load 'lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("crystalline"))
;;                     :activation-fn (lsp-activate-on "crystal")
;;                     :priority '1
;;                     :server-id 'crystalline)))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(crystal-mode . "crystal"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("crystalline"))
                    :activation-fn (lsp-activate-on "crystal")
                    :priority '1
                    :server-id 'crystalline)))

(require 'lsp-mode_init)
(add-hook 'crystal-mode-hook 'lsp-mode-common-hooks)

;; (require 'dap-crystal_init)

(provide 'lsp-crystal_init)

;;; lsp-crystal_init.el ends here
