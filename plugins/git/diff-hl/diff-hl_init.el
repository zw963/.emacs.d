;; -*- lexical-binding: t; -*-

(require 'diff-hl)

(setq vc-git-diff-switches '("--histogram"))

;; (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(global-diff-hl-mode 1)

(require 'diff-hl-flydiff)
(diff-hl-flydiff-mode 1)

(require 'diff-hl-dired)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(require 'diff-hl-show-hunk)
;; Ctrl v * active diff-hl-show-hunk, or use mouse
(global-diff-hl-show-hunk-mouse-mode 1)

(setq diff-hl-show-staged-changes t)
(setq diff-hl-ask-before-revert-hunk t)
(setq diff-hl-disable-on-remote t)

(setq-default fringes-outside-margins t)

(setq diff-hl-fringe-bmp-function
      (lambda (&rest _)
        (define-fringe-bitmap 'my-diff-hl-bmp
          (vector #b00000000)
          1 8
          '(center t))))

(with-eval-after-load 'flymake
  (setq flymake-fringe-indicator-position 'right-fringe))

(defun diff-hl-revert-narrow-to-hunk-hacked (end)
  (if (fboundp 'fancy-narrow-to-region)
      (fancy-narrow-to-region (point) end)
    (narrow-to-region (point) end)))

(setq diff-hl-highlight-revert-hunk-function #'diff-hl-revert-narrow-to-hunk-hacked)

(defun diff-hl-show-hunk-or-show-first-hunk ()
  (interactive)
  (if (diff-hl-hunk-overlay-at (point))
      (diff-hl-show-hunk)
    (progn
      (goto-char (point-min))
      (diff-hl-show-hunk-next))
    ))

(define-key diff-hl-command-map [(*)] 'diff-hl-show-hunk-or-show-first-hunk)

(with-eval-after-load 'ws-butler
  (advice-add #'ws-butler-after-save :after #'diff-hl-update-once))

;; 一个有用的快捷键是：鼠标点击左侧 fringe, diff-hl-show-hunk--click

(provide 'diff-hl_init)

;;; diff-hl_init.el ends here
