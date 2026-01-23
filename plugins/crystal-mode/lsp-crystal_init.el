;; -*- lexical-binding: t; -*-

;; 注意，这里不要首先 require lsp-crystal, 否则将用默认的 scry 作为 server.
;; (setq lsp-clients-crystal-executable '("liger"))
(require 'lsp-crystal)
(require 'lsp-mode_init)

(add-hook 'crystal-mode-hook 'lsp-mode-common-hooks)

;; (require 'dap-crystal_init)

(provide 'lsp-crystal_init)

;;; lsp-crystal_init.el ends here
