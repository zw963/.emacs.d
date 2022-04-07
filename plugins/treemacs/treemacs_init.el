(require 'cfrs)
(require 'treemacs)
(require 'treemacs-file-management)

(treemacs-git-mode 'deferred)
(treemacs-filewatch-mode t)
;; (setq treemacs-file-event-delay 1000)

;; we don't need treemacs command, use q to close and exit treemacs.
(global-set-key [(f9)] 'treemacs)
(setq treemacs-show-hidden-files nil) ; 快捷键 th
;; (setq treemacs-width-is-initially-locked nil) ; 快捷键 tw

(treemacs-fringe-indicator-mode 'always)
(treemacs-indent-guide-mode t)
(treemacs-follow-mode t)
;; (require 'treemacs-project-follow-mode)
;; (treemacs-project-follow-mode t)
(setq treemacs-is-never-other-window t)
(setq treemacs-silent-refresh    t)
(setq treemacs-silent-filewatch    t)

;; (defun treemacs-ignore-example (filename absolute-path)
;;   (or (string-suffix-p filename ".elc")
;;       ;; (string-prefix-p "/x/y/z/" absolute-path)
;;       ))

;; (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-example)

(global-set-key [(control x) (\1)] 'treemacs-delete-other-windows)

(setq treemacs-width 42)

(require 'treemacs-icons-dired)
;; 让 dired 使用 treemacs 图标。
(treemacs-icons-dired-mode t)

;; (require 'treemacs-all-the-icons)
;; (treemacs-load-theme "all-the-icons")

;; 改变图标大小，默认 22.
;; (treemacs-resize-icons 18)

;; (setq treemacs-indentation-string (propertize " ⫶ " 'face 'font-lock-comment-face)
;;       treemacs-indentation 1)

(add-hook 'treemacs-mode-hook '(lambda ()
                                 (with-eval-after-load 'doom-themes
                                   (require 'doom-themes-ext-treemacs)
                                   (doom-themes-treemacs-config)
                                   )
                                 (define-key treemacs-mode-map [(control d)] 'treemacs-remove-project-from-workspace)
                                 ))

(with-eval-after-load 'lsp-mode
  ;; (add-hook 'treemacs-switch-workspace-hook 'lsp-treemacs-symbols)
  ;; (add-hook 'treemacs-select-hook 'lsp-ui-imenu)
  ;; (add-hook 'treemacs-switch-workspace-hook 'lsp-ui-imenu)
  (defun lsp-mode-common-hooks ()
    (when (featurep 'treemacs) (save-selected-window (treemacs-select-window)))
    )
  ;; (add-hook 'lsp-mode-hook 'lsp-mode-common-hooks)
  )


(provide 'treemacs_init)
;;; treemacs_init.el ends here
