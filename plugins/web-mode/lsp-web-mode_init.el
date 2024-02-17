(require 'lsp-tailwindcss)
(require 'lsp-mode_init)

(add-to-list 'lsp-language-id-configuration '("\\.erb$" . "html"))
(add-to-list 'lsp-language-id-configuration '("\\.ecr$" . "html"))
(setq lsp-tailwindcss-add-on-mode t)

(add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)
(add-to-list 'lsp-tailwindcss-major-modes 'crystal-mode)

(add-hook 'web-mode-hook 'lsp-mode-common-hooks)

(provide 'lsp-web-mode_init)

;;; lsp-web-mode_init.el ends here
