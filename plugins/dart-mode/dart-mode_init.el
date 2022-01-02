;; (require 'project)

;; (defun project-try-dart (dir)
;;   (let ((project (or (locate-dominating-file dir "pubspec.yaml")
;;                      (locate-dominating-file dir "BUILD"))))
;;     (if project
;;         (cons 'dart project)
;;       (cons 'transient dir))))
;; (add-hook 'project-find-functions #'project-try-dart)
;; (cl-defmethod project-root ((project (head dart)))
;;   (list (cdr project)))

(require 'dart-mode)
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

(with-eval-after-load 'lsp-mode
  ;; (setq lsp-signature-auto-activate nil)
  (setq lsp-dart-dap-flutter-hot-reload-on-save t)
  (add-hook 'dart-mode-hook 'lsp)
  )

;; (require 'flutter)
;; (setq flutter-l10n-arb-dir "lib/i10n")
;; (setq flutter-l10n-template-arb-file "intl_zh_Hans.arb")

;; ;; (add-hook 'dart-mode-hook 'flutter-test-mode)
;; (define-key dart-mode-map (kbd "C-M-x") 'flutter-run-or-hot-reload)

;; (require 'hover)

;; (setq hover-command-path "hover")
;; (setq hover-hot-reload-on-save t)
;; (setq hover-clear-buffer-on-hot-restart t)
;; (setq hover-screenshot-path "$HOME/Pictures")

(provide 'dart-mode_init)

;;; dart-mode_init.el ends here
