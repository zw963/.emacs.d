;; -*- lexical-binding: t; -*-

(require 'lsp-mode_init)
(require 'lsp-tailwindcss)

(setq lsp-tailwindcss-add-on-mode t)
(setq lsp-tailwindcss-rustywind-extra-args '("--custom-regex" "class(?:=|:\s*)(?:'|\")([^\"']+?)(?:'|\")"))
;; (add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)

(add-to-list 'lsp-language-id-configuration '("\\.erb$" . "html"))
(add-to-list 'lsp-language-id-configuration '("\\.ecr$" . "html"))
(add-hook 'web-mode-hook 'lsp-mode-common-hooks)

;; (add-to-list 'lsp-tailwindcss-major-modes 'crystal-mode)

(dolist (hook '(crystal-mode-hook web-mode-hook))
  (add-hook hook (lambda ()
                   (add-hook 'before-save-hook #'lsp-tailwindcss-rustywind nil t)
                   )))

;; (add-to-list 'lsp-language-id-configuration '("\\.slim$" . "html"))
;; (add-to-list 'lsp-tailwindcss-major-modes 'slim-mode)

(provide 'tailwindcss_init)

;;; tailwindcss_init.el ends here
