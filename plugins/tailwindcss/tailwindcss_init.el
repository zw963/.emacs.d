(require 'lsp-tailwindcss)

(setq lsp-tailwindcss-add-on-mode t)
(add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)

(add-to-list 'lsp-language-id-configuration '("\\.erb$" . "html"))
(add-to-list 'lsp-language-id-configuration '("\\.ecr$" . "html"))
(add-to-list 'lsp-language-id-configuration '("\\.slim$" . "html"))
(add-to-list 'lsp-tailwindcss-major-modes 'crystal-mode)
(add-to-list 'lsp-tailwindcss-major-modes 'slim-mode)

(provide 'tailwindcss_init)

;;; tailwindcss_init.el ends here
