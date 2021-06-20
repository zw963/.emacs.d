(require 'diff-hl)
;; (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(global-diff-hl-mode 1)
(require 'diff-hl-dired)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(require 'diff-hl-show-hunk)
(global-diff-hl-show-hunk-mouse-mode 1)
;; (require 'diff-hl-show-hunk-posframe)
;; (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-posframe)

(provide 'diff-hl_init)

;;; diff-hl_init.el ends here
