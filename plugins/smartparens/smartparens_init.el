(require 'smartparens-config)

(sp-local-pair 'sh-mode "'" "'" :unless nil)
(smartparens-global-mode 1)

(add-hook 'prog-mode-hook 'smartparens-global-strict-mode)

(provide 'smartparens_init)

;;; smartparens_init.el ends here
