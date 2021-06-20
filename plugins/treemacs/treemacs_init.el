(require 'treemacs)

(treemacs-git-mode 'deferred)
;; we don't need treemacs command, use q to close and exit treemacs.
(global-set-key [(f9)] 'treemacs-select-window)
(setq treemacs-show-hidden-files nil)

(treemacs-fringe-indicator-mode t)
(treemacs-filewatch-mode t)
(treemacs-follow-mode t)
(treemacs-git-mode 'deferred)
(setq treemacs-is-never-other-window t)
(setq treemacs-silent-refresh    t)

;; (setq treemacs-width 30)

(require 'treemacs-icons-dired)
;; 让 dired 使用 treemacs 图标。
(treemacs-icons-dired-mode t)

;; 改变图标大小，默认 22.
;; (treemacs-resize-icons 22)

;; (setq treemacs-indentation-string (propertize " ⫶ " 'face 'font-lock-comment-face)
;;       treemacs-indentation 1)

(provide 'treemacs_init)
;;; treemacs_init.el ends here
