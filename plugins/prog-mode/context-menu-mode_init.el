(require 'hideshow)
(require 'eldoc-box_init)

(setq-default context-menu-functions
              '(context-menu-hideshow
                occur-context-menu
                context-menu-region
                ))

(defun context-menu-hideshow (menu click)
  "Populate MENU with `hideshow' commands."
  (save-excursion
    (mouse-set-point click)
    (if (hs-already-hidden-p)
        (define-key-after menu [hs-show-block]
          '(menu-item "Show block"
                      (lambda (click) (interactive "e")
                        (save-excursion
                          (mouse-set-point click)
                          (hs-show-block)))))
      (define-key-after menu [hs-hide-block]
        '(menu-item "Hide block"
                    (lambda (click) (interactive "e")
                      (save-excursion
                        (mouse-set-point click)
                        (hs-hide-block)))))))
  (define-key-after menu [hs-separator]
    menu-bar-separator)
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

(defun context-menu-set-lsp-common-context-menu ()
  (setq context-menu-functions
        '(context-menu-hideshow
          context-menu-show-eldoc
          context-menu-show-lsp-code-actions
          context-menu-unwrap-flutter-widget
          occur-context-menu
          prog-context-menu
          context-menu-local
          )))

(add-hook 'dart-mode-hook 'context-menu-set-lsp-common-context-menu)
(add-hook 'go-mode-hook 'context-menu-set-lsp-common-context-menu)

(context-menu-mode t)

(provide 'context-menu-mode_init)

;;; context-menu-mode_init.el ends here
