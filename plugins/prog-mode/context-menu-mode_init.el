(require 'hideshow_init)
(require 'yafolding_init)

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
      '(menu-item "Close current Window"
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
          '(menu-item "Unfold block"
                      (lambda (click) (interactive "e")
                        (save-excursion
                          (mouse-set-point click)
                          (hs-show-block)))))
      (define-key-after menu [hs-hide-block]
        '(menu-item "Fold block"
                    (lambda (click) (interactive "e")
                      (save-excursion
                        (mouse-set-point click)
                        (hs-hide-block)))))))
  (define-key-after menu [hs-separator]
    menu-bar-separator)
  menu)

;; For use with yaml-mode
(defun context-menu-yafolding (menu click)
  "Populate MENU with `hideshow' commands."
  (save-excursion
    (mouse-set-point click)
    (if (yafolding-get-overlays (line-beginning-position)
                                (+ 1 (line-end-position)))
        (define-key-after menu [hs-show-block]
          '(menu-item "Unfold indentation"
                      (lambda (click) (interactive "e")
                        (save-excursion
                          (mouse-set-point click)
                          (yafolding-show-element)))))
      (define-key-after menu [hs-hide-block]
        '(menu-item "Fold indentation"
                    (lambda (click) (interactive "e")
                      (save-excursion
                        (mouse-set-point click)
                        (yafolding-hide-element)))))))
  (define-key-after menu [hs-separator]
    menu-bar-separator)
  menu)

(defun context-menu-show-git-message (menu click)
  "Populate MENU with `git-messenger' commands."
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

(defun context-menu-show-lsp-describe-thing-at-point (menu click)
  "Populate MENU with `lsp describe thing at point' commands."
  (define-key-after menu [show-lsp-code-actions]
    '(menu-item "Show lsp describe thing at point"
                (lambda (click) (interactive "e")
                  (save-excursion
                    (mouse-set-point click)
                    (call-interactively 'lsp-describe-thing-at-point)
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

(dolist (hook '(
                yaml-mode-hook
                yaml-ts-mode-hook
                ))
  (add-hook hook (lambda ()
                   (add-to-list 'context-menu-functions 'context-menu-split/close-window)
                   (add-to-list 'context-menu-functions 'context-menu-show-git-message)

                   (add-to-list 'context-menu-functions 'context-menu-yafolding)
                   )))

(dolist (hook '(
                dart-mode-hook
                ))
  (add-hook hook (lambda ()
                   (add-to-list 'context-menu-functions 'context-menu-unwrap-flutter-widget)

                   (add-to-list 'context-menu-functions 'context-menu-show-lsp-describe-thing-at-point)
                   (add-to-list 'context-menu-functions 'context-menu-show-lsp-code-actions)

                   (add-to-list 'context-menu-functions 'context-menu-split/close-window)
                   (add-to-list 'context-menu-functions 'context-menu-show-git-message)

                   (add-to-list 'context-menu-functions 'context-menu-hideshow)
                   )))

;; (dolist (hook '(
;;                 rust-mode-hook
;;                 rust-ts-mode-hook
;;                 rustic-mode-hook
;;                 ))
;;   (add-hook hook 'add-lsp-common-context-menu))

;; 这里可以使用 hideshow 的 mode 都必须在 hs-special-modes-alist 当中被定义
(dolist (hook '(
                ruby-mode-hook
                ruby-ts-mode-hook
                enh-ruby-mode-hook
                crystal-mode-hook
                elixir-ts-mode-hook
                go-mode-hook
                mint-mode-hook
                bash-ts-mode-hook
                emacs-lisp-mode-hook
                ))
  (add-hook hook (lambda ()
                   (add-to-list 'context-menu-functions 'context-menu-show-lsp-describe-thing-at-point)
                   (add-to-list 'context-menu-functions 'context-menu-show-lsp-code-actions)
                   (add-to-list 'context-menu-functions 'context-menu-split/close-window)
                   (add-to-list 'context-menu-functions 'context-menu-show-git-message)
                   (add-to-list 'context-menu-functions 'context-menu-hideshow)
                   )))

(context-menu-mode t)

(provide 'context-menu-mode_init)

;;; context-menu-mode_init.el ends here
