(require 'rust-mode_init)
;; (require 'rustic_init)
(require 'lsp-rust-mode_init)
;; (require 'rust-mode-eglot_init)

(autoload 'toml-mode "toml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))
(add-to-list 'auto-mode-alist '("\\`\\(rust-toolchain\\)\\'" . toml-mode))

(provide 'rust_init)

;;; rust_init.el ends here
