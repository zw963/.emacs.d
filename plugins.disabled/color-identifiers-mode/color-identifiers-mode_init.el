(require 'color-identifiers-mode)

(add-to-list
 'color-identifiers:modes-alist
 `(enh-ruby-mode . (,color-identifiers:re-not-inside-class-access "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)" (nil))))

(add-hook 'ruby-mode-hook 'color-identifiers-mode)
(add-hook 'enh-ruby-mode-hook 'color-identifiers-mode)
(add-hook 'emacs-lisp-mode-hook 'color-identifiers-mode)
(add-hook 'rust-mode-hook 'color-identifiers-mode)
(add-hook 'go-mode-hook 'color-identifiers-mode)
(add-hook 'js-mode-hook 'color-identifiers-mode)
(add-hook 'js2-mode-hook 'color-identifiers-mode)

(provide 'color-identifiers-mode_init)

;;; color-identifiers-mode_init.el ends here
