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

(require 'lsp-dart)
(setq lsp-signature-auto-activate nil)
;; (setq lsp-dart-dap-flutter-hot-reload-on-save t)
(define-key dart-mode-map (kbd "C-M-x") 'lsp-dart-dap-flutter-hot-reload)
(define-key dart-mode-map (kbd "<escape>") 'lsp-dart-show-flutter-outline)
(add-hook 'dart-mode-hook 'lsp-deferred)

(with-eval-after-load 'treemacs
  (add-hook
   'dart-mode-hook
   (lambda ()
     (save-selected-window (treemacs-select-window))))
  )

(require 'flutter)
(setq flutter-l10n-arb-dir "lib/i10n")
(setq flutter-l10n-template-arb-file "intl_zh_Hans.arb")
(setq flutter-l10n-output-localization-file "l10n.dart")

(defun use-charles-proxy ()
  (interactive)
  (setenv "http_proxy" "http://127.0.0.1:8888")
  (setenv "https_proxy" "http://127.0.0.1:8888")
  (message "set http_proxy https_proxy to http://127.0.0.1:8888")
  )

;; (add-hook 'dart-mode-hook 'flutter-test-mode)
;; (define-key dart-mode-map (kbd "C-M-x") 'flutter-run-or-hot-reload)

(require 'hover)

(with-eval-after-load 'hover
  (define-key hover-minor-mode-map (kbd "C-M-x") 'hover-run-or-hot-restart)
  (setq
   hover-screenshot-path (concat (getenv "HOME") "/Pictures")
   hover-screenshot-prefix "magpie-"
   hover-observatory-uri "http://127.0.0.1:50300"
   hover-clear-buffer-on-hot-restart t
   hover-hot-reload-on-save t
   )
  (hover-global-minor-mode t)
  )

(provide 'dart-mode_init)

;;; dart-mode_init.el ends here
