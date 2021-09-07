(require 'cfrs)
(require 'treemacs)

(treemacs-git-mode 'deferred)
;; we don't need treemacs command, use q to close and exit treemacs.
(global-set-key [(f9)] 'treemacs-select-window)
(setq treemacs-show-hidden-files nil)

(treemacs-fringe-indicator-mode t)
(treemacs-filewatch-mode t)
(setq treemacs-file-event-delay 1000)
;; (treemacs-follow-mode t)
;; (require 'treemacs-project-follow-mode)
;; (treemacs-project-follow-mode t)
(treemacs-git-mode 'deferred)
(setq treemacs-is-never-other-window t)
(setq treemacs-silent-refresh    t)

(defun treemacs-ignore-example (filename absolute-path)
  (or (string-suffix-p filename ".elc")
      ;; (string-prefix-p "/x/y/z/" absolute-path)
      ))

(add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-example)

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
