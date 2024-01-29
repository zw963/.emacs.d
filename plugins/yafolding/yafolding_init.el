(require 'yafolding)

(with-eval-after-load 'all-the-icons
  (setq yafolding-ellipsis-content (all-the-icons-material "unfold_more"))
  )

(add-hook 'nxml-mode-hook 'yafolding-mode)
(add-hook 'yaml-mode-hook 'yafolding-mode)
(add-hook 'yaml-ts-mode-hook 'yafolding-mode)

(define-key yafolding-mode-map [(control tab)] 'yafolding-toggle-element)

(provide 'yafolding_init)

;;; yafolding_init.el ends here

