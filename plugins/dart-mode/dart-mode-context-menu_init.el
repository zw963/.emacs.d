(require 'eldoc-box_init)

(defun context-menu-unwrap-flutter-widget (menu click)
  "Populate MENU with `flutter-unwrap-widget' commands."
  (define-key-after menu [unwrap-flutter-widget]
    '(menu-item "Unwrap widget"
                (lambda (click) (interactive "e")
                  (save-excursion
                    (mouse-set-point click)
                    (flutter-unwrap-widget)))))
  (define-key-after menu [hs-separator] menu-bar-separator)
  menu)

(defun context-menu-show-eldoc (menu click)
  "Populate MENU with `eldoc-box-eglot-help-at-point' commands."
  (define-key-after menu [show-eldoc]
    '(menu-item "Show eldoc documentation"
                (lambda (click) (interactive "e")
                  (save-excursion
                    (if (featurep 'lsp-mode)
                        (lsp-describe-thing-at-point)
                      (eldoc-box-eglot-help-at-point))))))
  (define-key-after menu [hs-separator] menu-bar-separator)
  menu)

(defun context-menu-show-lsp-code-actions (menu click)
  "Populate MENU with `lsp code action' commands."
  (define-key-after menu [show-lsp-code-actions]
    '(menu-item "Show lsp code actions"
                (lambda (click) (interactive "e")
                  (save-excursion
                    (mouse-set-point click)
                    (if (featurep 'lsp-mode)
                        (call-interactively 'lsp-execute-code-action)
                      (call-interactively 'eglot-code-actions))
                    ))))
  (define-key-after menu [hs-separator] menu-bar-separator)
  menu)

(add-hook 'dart-mode-hook
          (lambda ()
            (setq context-menu-functions
                  '(context-menu-hideshow
                    context-menu-show-eldoc
                    context-menu-show-lsp-code-actions
                    context-menu-unwrap-flutter-widget
                    occur-context-menu
                    prog-context-menu
                    context-menu-local
                    ))
            ))

(provide 'dart-mode-context-menu_init)

;;; dart-mode-context-menu_init.el ends here