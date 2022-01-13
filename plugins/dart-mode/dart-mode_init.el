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
  (setq lsp-signature-auto-activate nil)
  ;; (setq lsp-dart-dap-flutter-hot-reload-on-save t)
  (define-key dart-mode-map (kbd "C-M-x") 'lsp-dart-dap-flutter-hot-reload)
  (add-hook 'dart-mode-hook 'lsp)
  ;; disable treemacs-follow-mode when run lsp-dart-run
(advice-add #'lsp-dart-run :after (lambda (&rest _) (setq-local treemacs-follow-mode nil)))
  )

(with-eval-after-load 'treemacs
  (add-hook
   'dart-mode-hook
   (lambda ()
     (save-selected-window (treemacs-select-window))))
  )

(require 'flutter)
(setq flutter-l10n-arb-dir "lib/i10n")
(setq flutter-l10n-template-arb-file "intl_zh_Hans.arb")

;; (add-hook 'dart-mode-hook 'flutter-test-mode)
;; (define-key dart-mode-map (kbd "C-M-x") 'flutter-run-or-hot-reload)

(require 'hover)

(with-eval-after-load 'hover
  ;; (define-key dart-mode-map (kbd "C-M-x") 'hover-run-or-hot-reload)
  ;; (define-key dart-mode-map (kbd "C-M-z") 'hover-run-or-hot-restart)
  ;; (define-key dart-mode-map (kbd "C-M-p") 'hover-take-screenshot)

  (setq
   hover-screenshot-path (concat (getenv "HOME") "/Pictures")
   hover-screenshot-prefix "magpie-"
   hover-observatory-uri "http://127.0.0.1:50300"
   hover-clear-buffer-on-hot-restart t
   hover-hot-reload-on-save t
        )
  )

(provide 'dart-mode_init)

;;; dart-mode_init.el ends here
