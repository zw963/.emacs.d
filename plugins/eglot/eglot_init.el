(require 'eglot)

;; (add-to-list 'eglot-server-programs '(enh-ruby-mode . ("solargraph" "socket" "--port" :autoport)))
;; (run-ruby-mode-hook '(eglot-ensure))

(add-hook 'before-save-hook 'eglot-format-buffer)

;; (define-key eglot-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)

;; (stq eglot-confirm-server-initiated-edits nil)

(require 'eglot-flycheck_init)

(provide 'eglot_init)

;;; eglot_init.el ends here