;; -*- lexical-binding: t; -*-

(require 'lsp-mode_init)
(require 'lsp-tailwindcss)

(setq lsp-tailwindcss-add-on-mode t)

;; 允许匹配的格式，使用 classes capture 所有 class names
;; - class="foo bar"
;; - class='foo bar'
;; - class: "foo bar"
;; - class:'foo bar'
(setq lsp-tailwindcss-rustywind-command (executable-find "rustywind-lucky"))

;; (add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)

(add-hook 'web-mode-hook 'lsp-mode-common-hooks)

(add-to-list 'lsp-tailwindcss-major-modes 'crystal-mode)

(dolist (hook '(crystal-mode-hook web-mode-hook css-ts-mode-hook))
  (add-hook hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-tailwindcss-rustywind nil t)
              )))

;; (add-to-list 'lsp-language-id-configuration '("\\.slim$" . "html"))
;; (add-to-list 'lsp-tailwindcss-major-modes 'slim-mode)

(provide 'tailwindcss_init)

;;; tailwindcss_init.el ends here
