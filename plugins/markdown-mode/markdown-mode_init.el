;; markdown-mode
(require 'markdown-mode)

(setq markdown-command "pulldown-cmark"
      markdown-open-command "pulldown-cmark"
      markdown-gfm-use-electric-backquote nil
      markdown-indent-on-enter 'indent-and-new-item
      markdown-content-type "text/html"
      markdown-coding-system 'utf-8
      markdown-gfm-uppercase-checkbox t
      markdown-gfm-use-electric-backquote nil
      markdown-hide-urls t
      )

(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.mkdn$\\|\\.mkd$\\|\\.mdown$" . gfm-mode))

(add-hook 'markdown-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-x n"))
            (define-key markdown-mode-map [(control c) (return)] 'markdown-preview)
            (define-key markdown-mode-map [(meta c) (n)] 'markdown-narrow-to-subtree)
            (define-key markdown-mode-map [(meta n)] 'window-move-up)
            (define-key markdown-mode-map [(meta p)] 'window-move-down)
            (define-key edit-indirect-mode-map (kbd "C-c C-c") #'format-buffer)
            (whitespace-mode -1)
            (display-line-numbers-mode)
            (display-fill-column-indicator-mode)
            ))

(provide 'markdown-mode_init)
;;; markdown-mode_init.el ends here
