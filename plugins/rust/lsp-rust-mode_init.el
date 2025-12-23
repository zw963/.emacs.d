;; -*- lexical-binding: t; -*-

;; lsp-rust.el 做的核心事情是 lsp-register-client。只要你对 Rust buffer 调了
;; lsp / lsp-deferred，lsp-mode 机制会确保对应 client 被加载并参与匹配.
;;（Rust 的 activation 是 (lsp-activate-on "rust")）。

(require 'lsp-mode_init)

(setq lsp-rust-analyzer-display-chaining-hints t)
(setq lsp-rust-full-docs t)

;; 注意，如果用 rls, 必须把 lsp-enable-imenu 设为 nil, ruby-analyzer 无需。
;; (setq-local lsp-enable-imenu nil)

(add-hook 'rust-mode-hook 'lsp-mode-common-hooks)

(provide 'lsp-rust-mode_init)

;;; lsp-rust-mode_init.el ends here
