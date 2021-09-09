(require 'pangu-spacing)

(add-hook 'org-mode-hook
          '(lambda ()
             (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)
             (pangu-spacing-mode 1)
             ))

(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)
             (pangu-spacing-mode 1)
             ))

(provide 'pangu-spacing_init)

;;; pangu-spaing_init.el ends here
