(require 'pangu-spacing)

(global-pangu-spacing-mode 1)

(add-hook 'org-mode-hook
          '(lambda ()
             (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

(provide 'pangu-spacing_init)

;;; pangu-spaing_init.el ends here
