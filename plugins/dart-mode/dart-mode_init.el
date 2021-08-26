(require 'project)
(defun project-try-dart (dir)
  (let ((project (or (locate-dominating-file dir "pubspec.yaml")
                     (locate-dominating-file dir "BUILD"))))
    (if project
        (cons 'dart project)
      (cons 'transient dir))))
(add-hook 'project-find-functions #'project-try-dart)
(cl-defmethod project-roots ((project (head dart)))
  (list (cdr project)))

(require 'dart-mode)
(require 'pkg-info) ;; lsp-dart 依赖这个，来让启动 server 正常工作，非常坑。
(require 'lsp-dart)
(require 'lsp-dart-commands)
(require 'lsp-dart-devtools)
(require 'lsp-dart-outline)
(require 'lsp-dart-flutter-daemon)
(lsp-dart-flutter-daemon-mode)
(require 'lsp-dart-test-support)

(setq lsp-signature-auto-activate nil)

(setq dart-format-on-save t)

(add-hook 'dart-mode-hook 'lsp)

(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

(provide 'dart-mode_init)

;;; dart-mode_init.el ends here
