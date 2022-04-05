(require 'eglot)

;; (add-to-list 'eglot-server-programs '(enh-ruby-mode . ("solargraph" "socket" "--port" :autoport)))
;; (run-ruby-mode-hook '(eglot-ensure))

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

;; (define-key eglot-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)

;; (stq eglot-confirm-server-initiated-edits nil)

(require 'eglot-flycheck_init)

(provide 'eglot_init)

;;; eglot_init.el ends here
