(require 'eldoc-box)

(setq eldoc-box-clear-with-C-g t)

(with-eval-after-load 'eglot
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc-box-eglot-help-at-point)
  )

(provide 'eldoc-box_init)

;;; eldoc-box_init.el ends here
