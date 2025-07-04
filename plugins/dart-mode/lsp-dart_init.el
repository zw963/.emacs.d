;; -*- lexical-binding: t; -*-

(require 'lsp-mode_init)
(require 'lsp-dart)

(defun special-mode-lsp-dart-dap-flutter-hot-reload ()
  (interactive)
  (goto-char (point-max))
  (lsp-dart-dap-flutter-hot-reload))

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

;; (setq lsp-dart-dap-use-sdk-debugger nil)

(add-hook 'crystal-mode-hook 'lsp-mode-common-hooks)

(provide 'lsp-dart_init)

;;; name_init.el ends here
