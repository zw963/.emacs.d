(require 'diff-hl)

;; (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(global-diff-hl-mode 1)

(require 'diff-hl-dired)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(require 'diff-hl-show-hunk)
;; Ctrl v * active diff-hl-show-hunk, or use mouse
(global-diff-hl-show-hunk-mouse-mode 1)

(require 'diff-hl-flydiff)
(diff-hl-flydiff-mode 1)

(defun diff-hl-show-hunk-or-show-first-hunk ()
  (interactive)
  (if (diff-hl-hunk-overlay-at (point))
      (diff-hl-show-hunk)
    (progn
      (beginning-of-buffer)
      (diff-hl-show-hunk-next))
    ))

(define-key diff-hl-command-map [(*)] 'diff-hl-show-hunk-or-show-first-hunk)
(define-key diff-hl-show-hunk--inline-popup-map (kbd "p") #'diff-hl-show-hunk-previous)
(define-key diff-hl-show-hunk--inline-popup-map (kbd "n") #'diff-hl-show-hunk-next)
(define-key diff-hl-show-hunk--inline-popup-map (kbd "r") #'diff-hl-show-hunk-revert-hunk)

;; (require 'diff-hl-show-hunk-posframe)
;; (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-posframe)

(provide 'diff-hl_init)

;;; diff-hl_init.el ends here
