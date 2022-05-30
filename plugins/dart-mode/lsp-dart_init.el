(require 'lsp-mode_init)
(require 'dap-mode_init)
(require 'lsp-dart)

(defun special-mode-lsp-dart-dap-flutter-hot-restart ()
  (interactive)
  (goto-char (point-max))
  (lsp-dart-dap-flutter-hot-restart))

;; (setq lsp-dart-dap-flutter-hot-reload-on-save t)
(define-key dart-mode-map (kbd "C-M-x") 'lsp-dart-dap-flutter-hot-reload)
(define-key dart-mode-map (kbd "C-M-z") 'lsp-dart-dap-flutter-hot-restart)
(define-key special-mode-map (kbd "C-M-x") 'special-mode-lsp-dart-dap-flutter-hot-reload)
(define-key special-mode-map (kbd "C-M-z") 'special-mode-lsp-dart-dap-flutter-hot-restart)
;; (define-key dart-mode-map (kbd "<escape>") 'lsp-dart-show-flutter-outline)

(setq lsp-dart-dap-extension-version "3.38.1")
(setq lsp-dart-dap-use-sdk-debugger nil)

(add-hook 'dart-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer t t)
            ;; (lsp-deferred)
            ))

(provide 'lsp-dart_init)

;;; name_init.el ends here
