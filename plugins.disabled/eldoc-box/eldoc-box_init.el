(require 'eldoc-box)

(setq eldoc-box-clear-with-C-g t)

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


(with-eval-after-load 'eglot
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc-box-eglot-help-at-point)
  )

(provide 'eldoc-box_init)

;;; eldoc-box_init.el ends here
