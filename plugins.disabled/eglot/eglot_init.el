(require 'eglot)

(add-to-list 'eglot-server-programs '(enh-ruby-mode . ("solargraph" "socket" "--port" :autoport)))
(setq eglot-autoreconnect 3)
(ruby-ruby-mode-hook '(eglot-ensure))
(define-key eglot-mode-map (kbd "s-j") 'eglot-help-at-point)

(provide 'eglot_init)

;;; eglot_init.el ends here