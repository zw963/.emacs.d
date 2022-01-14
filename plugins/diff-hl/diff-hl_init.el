(require 'diff-hl)

;; (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(global-diff-hl-mode 1)

(require 'diff-hl-flydiff)
(diff-hl-flydiff-mode 1)

(require 'diff-hl-dired)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(require 'diff-hl-show-hunk)
;; Ctrl v * active diff-hl-show-hunk, or use mouse
(global-diff-hl-show-hunk-mouse-mode 1)

;; (setq diff-hl-show-staged-changes t)
;; (setq diff-hl-ask-before-revert-hunk t)

;; (defun diff-hl-revert-narrow-to-hunk (end)
;;   (if (fboundp 'fancy-narrow-to-region)
;;       (fancy-narrow-to-region (point) end)
;;     (narrow-to-region (point) end)))

;; (setq diff-hl-highlight-revert-hunk-function #'diff-hl-revert-narrow-to-hunk)

(defun diff-hl-show-hunk-or-show-first-hunk ()
  (interactive)
  (if (diff-hl-hunk-overlay-at (point))
      (diff-hl-show-hunk)
    (progn
      (goto-char (point-min))
      (diff-hl-show-hunk-next))
    ))

(define-key diff-hl-command-map [(*)] 'diff-hl-show-hunk-or-show-first-hunk)

;; (require 'diff-hl-show-hunk-posframe)
;; (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-posframe)

(provide 'diff-hl_init)

;;; diff-hl_init.el ends here
