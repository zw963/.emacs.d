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
  (require 'lsp-dart)
  (require 'lsp-dart-dap)
  (require 'lsp-dart-commands)
  (require 'lsp-dart-devtools)
  (require 'lsp-dart-outline)
  (require 'lsp-dart-protocol)
  (require 'lsp-dart-utils)
  (require 'lsp-dart-code-lens)
  (require 'lsp-dart-closing-labels)

  ;; (require 'lsp-dart-flutter-daemon)
  ;; (lsp-dart-flutter-daemon-mode)
  (require 'lsp-dart-flutter-colors)
  (require 'lsp-dart-flutter-fringe-colors)
  (require 'lsp-dart-flutter-widget-guide)

  ;; lsp-dart-test-output.el
  ;; lsp-dart-test-support.el
  ;; lsp-dart-test-tree.el


  (defun lsp-dart-get-project-entrypoint ()
    "Return the dart or flutter project entrypoint."
    (let* ((root (lsp-dart-get-project-root))
           (lib-entry (expand-file-name "lib/main_local.dart" root))
           (bin-entry (expand-file-name "bin/main.dart" root)))
      (cond
       ((file-exists-p lib-entry)
        lib-entry)

       ((file-exists-p bin-entry)
        bin-entry))))

  (dap-register-debug-template
   "Flutter :: Custom run"
   (list :flutterPlatform "x86_64"
         :program "lib/main_local.dart"
         :type "flutter"
         ))

  (dap-register-debug-template
   "Flutter :: Custom debug"
   (list :flutterPlatform "x86_64"
         :program "lib/main_local.dart"
         :type "flutter"
         :flutterMode "debug"))

  (setq lsp-signature-auto-activate nil)
  (setq lsp-dart-dap-flutter-hot-reload-on-save t)
  (add-hook 'dart-mode-hook 'lsp)
  )

(require 'flutter)
(setq flutter-l10n-arb-dir "lib/i10n")
(setq flutter-l10n-template-arb-file "intl_zh_Hans.arb")

;; (add-hook 'dart-mode-hook 'flutter-test-mode)
(define-key dart-mode-map (kbd "C-M-x") 'flutter-run-or-hot-reload)

(require 'hover)

(setq hover-command-path "hover")
(setq hover-hot-reload-on-save t)
(setq hover-clear-buffer-on-hot-restart t)
(setq hover-screenshot-path "$HOME/Pictures")

(provide 'dart-mode_init)

;;; dart-mode_init.el ends here
