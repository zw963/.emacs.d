(require 'hideshow)
(require 'eldoc-box_init)

(setq-default context-menu-functions
              '(context-menu-hideshow
                context-menu-show-git-message
                context-menu-split/close-window
                occur-context-menu
                context-menu-region
                ))

(defun context-menu-split/close-window (menu click)
  "Close current window"
  (if (eq (count-visible-buffers) 1)
      (define-key-after menu [split-window]
        '(menu-item "Split Window"
                    (lambda (click) (interactive "e")
                      (save-excursion
                        (mouse-set-point click)
                        (split-window-right-then-switch-to)
                        ))))
    (define-key-after menu [close-window]
      '(menu-item "Close Window"
                  (lambda (click) (interactive "e")
                    (save-excursion
                      (mouse-set-point click)
                      (delete-window)
                      )))))

  (define-key-after menu [hs-separator] menu-bar-separator)
  menu)

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
                    (if (bound-and-true-p lsp-mode)
                        (lsp-describe-thing-at-point)
                      (eldoc-box-eglot-help-at-point))))))
  (define-key-after menu [hs-separator] menu-bar-separator)
  menu)

(defun context-menu-show-git-message (menu click)
  "Populate MENU with `eldoc-box-eglot-help-at-point' commands."
  (define-key-after menu [show-git-message]
    '(menu-item "Show git message"
                (lambda (click) (interactive "e")
                  (save-excursion
                    (mouse-set-point click)
                    (git-messenger:popup-message)
                    ))))
  (define-key-after menu [hs-separator] menu-bar-separator)
  menu)

(defun context-menu-show-lsp-code-actions (menu click)
  "Populate MENU with `lsp code action' commands."
  (define-key-after menu [show-lsp-code-actions]
    '(menu-item "Show lsp code actions"
                (lambda (click) (interactive "e")
                  (save-excursion
                    (mouse-set-point click)
                    (if (bound-and-true-p lsp-mode)
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

(defun context-menu-lsp-common-context-menu ()
  (setq context-menu-functions
        '(context-menu-hideshow
          context-menu-show-git-message
          context-menu-show-eldoc
          context-menu-show-lsp-code-actions
          occur-context-menu
          prog-context-menu
          context-menu-local
          )))

(defun context-menu-lsp-dart-context-menu ()
  (setq context-menu-functions
        '(context-menu-hideshow
          context-menu-show-git-message
          context-menu-show-eldoc
          context-menu-show-lsp-code-actions
          context-menu-unwrap-flutter-widget
          occur-context-menu
          prog-context-menu
          context-menu-local
          )))

(add-hook 'dart-mode-hook 'context-menu-lsp-dart-context-menu)
(add-hook 'go-mode-hook 'context-menu-lsp-common-context-menu)
(add-hook 'ruby-mode-hook 'context-menu-lsp-common-context-menu)
(add-hook 'enh-ruby-mode-hook 'context-menu-lsp-common-context-menu)
(add-hook 'rust-mode-hook 'context-menu-lsp-common-context-menu)
(add-hook 'rustic-mode-hook 'context-menu-lsp-common-context-menu)

(context-menu-mode t)

(provide 'context-menu-mode_init)

;;; context-menu-mode_init.el ends here
