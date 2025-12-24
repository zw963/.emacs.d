;; -*- lexical-binding: t; -*-

(require 'hideshow_init)
(require 'yafolding_init)
(require 'git-messenger_init)

(defmacro my/with-click-point (click &rest body)
  "Evaluate BODY with point set to CLICK position, preserving excursion."
  (declare (indent 1) (debug (form body)))
  `(save-excursion
     (mouse-set-point ,click)
     ,@body))

(defun context-menu-split/close-window (menu click)
  "Close current window"
  (if (eq (count-visible-buffers) 1)
      (define-key-after menu [split-window]
        '(menu-item "Split Window"
                    (lambda (click) (interactive "e")
                      (my/with-click-point click
                        (split-window-right-then-switch-to)
                        ))))
    (define-key-after menu [close-window]
      '(menu-item "Close current Window"
                  (lambda (click) (interactive "e")
                    (my/with-click-point click
                      (delete-window)
                      )))))
  (define-key-after menu [hs-separator] menu-bar-separator)
  menu)

(defun context-menu-hideshow (menu click)
  "Populate MENU with `hideshow' commands."
  (my/with-click-point click
    (if (hs-already-hidden-p)
        (define-key-after menu [hs-show-block]
          '(menu-item "Unfold"
                      (lambda (click) (interactive "e")
                        (my/with-click-point click
                          (hs-show-block)))))
      (define-key-after menu [hs-hide-block]
        '(menu-item "Fold"
                    (lambda (click) (interactive "e")
                      (my/with-click-point click
                        (hs-hide-block)))))))
  (define-key-after menu [hs-separator]
    menu-bar-separator)
  menu)

;; For use with yaml-mode
(defun context-menu-yafolding (menu click)
  "Populate MENU with `hideshow' commands."
  (my/with-click-point click
    (if (yafolding-get-overlays (line-beginning-position)
                                (+ 1 (line-end-position)))
        (define-key-after menu [hs-show-block]
          '(menu-item "Unfold"
                      (lambda (click) (interactive "e")
                        (my/with-click-point click
                          (yafolding-show-element)))))
      (define-key-after menu [hs-hide-block]
        '(menu-item "Fold"
                    (lambda (click) (interactive "e")
                      (my/with-click-point click
                        (yafolding-hide-element)))))))
  (define-key-after menu [hs-separator]
    menu-bar-separator)
  menu)

(defun context-menu-show-git-message (menu click)
  "Populate MENU with `git-messenger' commands."
  (define-key-after menu [show-git-message]
    '(menu-item "Show git message"
                (lambda (click) (interactive "e")
                  (my/with-click-point click
                    (cond
                     ((fboundp 'blamer-show-commit-info)
                      (blamer-show-commit-info))
                     ((fboundp 'git-messenger:popup-message)
                      (git-messenger:popup-message))
                     (t
                      (message "No git message backend available.")))))))
  (define-key-after menu [hs-separator] menu-bar-separator)
  menu)

(defun context-menu-show-lsp-code-actions (menu click)
  "Populate MENU with `lsp code action' commands."
  (define-key-after menu [show-lsp-code-actions]
    '(menu-item "Show lsp code actions"
                (lambda (click) (interactive "e")
                  (my/with-click-point click
                    (if (bound-and-true-p lsp-mode)
                        (call-interactively 'lsp-execute-code-action)
                      (call-interactively 'eglot-code-actions))
                    ))))
  (define-key-after menu [hs-separator] menu-bar-separator)
  menu)

(defun context-menu-show-lsp-describe-thing-at-point (menu click)
  "Populate MENU with `lsp describe thing at point' commands."
  (define-key-after menu [show-lsp-describe-thing-at-point]
    '(menu-item "Show lsp describe thing at point"
                (lambda (click) (interactive "e")
                  (my/with-click-point click
                    (cond
                     ((and (bound-and-true-p lsp-mode)
                           (fboundp 'lsp-describe-thing-at-point))
                      (call-interactively 'lsp-describe-thing-at-point))
                     ((fboundp 'eglot-help-at-point)
                      (call-interactively 'eglot-help-at-point))
                     (t
                      (message "No LSP/Eglot help function available.")))))))
  (define-key-after menu [hs-separator] menu-bar-separator)
  menu)

(defun context-menu-unwrap-flutter-widget (menu click)
  "Populate MENU with `flutter-unwrap-widget' commands."
  (define-key-after menu [unwrap-flutter-widget]
    '(menu-item "Unwrap widget"
                (lambda (click) (interactive "e")
                  (my/with-click-point click
                    (flutter-unwrap-widget)))))
  (define-key-after menu [hs-separator] menu-bar-separator)
  menu)

(defun my/context-menu-add (fn)
  (add-hook 'context-menu-functions fn nil t))

(dolist (hook '(
                yaml-mode-hook
                yaml-ts-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (my/context-menu-add 'context-menu-split/close-window)
              (my/context-menu-add 'context-menu-show-git-message)

              (my/context-menu-add 'context-menu-yafolding)
              )))

(dolist (hook '(
                dart-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (my/context-menu-add 'context-menu-unwrap-flutter-widget)

              (my/context-menu-add 'context-menu-show-lsp-describe-thing-at-point)
              (my/context-menu-add 'context-menu-show-lsp-code-actions)

              (my/context-menu-add 'context-menu-split/close-window)
              (my/context-menu-add 'context-menu-show-git-message)

              (my/context-menu-add 'context-menu-hideshow)
              (define-key lsp-mode-map [mouse-3] 'nil)
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
                rust-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (my/context-menu-add 'context-menu-show-lsp-describe-thing-at-point)
              (my/context-menu-add 'context-menu-show-lsp-code-actions)
              (my/context-menu-add 'context-menu-split/close-window)
              (my/context-menu-add 'context-menu-show-git-message)
              (my/context-menu-add 'context-menu-hideshow)
              ;; 开启 context-menu-mode 之后，lsp-mode 仍旧会在我们自定义的菜单之后，重新弹出 lsp 定义的右键菜单，关闭它
              ;; (define-key lsp-mode-map [mouse-3] 'nil)
              (keymap-set lsp-mode-map "<mouse-3>" #'ignore)
              )))

(context-menu-mode t)

(provide 'context-menu-mode_init)

;;; context-menu-mode_init.el ends here
