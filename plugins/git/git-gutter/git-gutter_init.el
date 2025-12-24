;; -*- lexical-binding: t; -*-

(require 'git-gutter)

(setq
 ;; git-gutter:hide-gutter t
 ;; git-gutter:window-width 2
 ;; git-gutter:ask-p nil
 git-gutter:visual-line t
 git-gutter:disabled-modes '(asm-mode image-mode)
 git-gutter:diff-option "-w"
 ;; git-gutter:update-interval 0.3 ;; 默认 0 异步的升级 diff 信息.
 )

(add-to-list 'git-gutter:update-commands 'ace-window)
(add-to-list 'git-gutter:update-commands 'windmove-left)
(add-to-list 'git-gutter:update-commands 'windmove-right)
(add-to-list 'git-gutter:update-commands 'windmove-up)
(add-to-list 'git-gutter:update-commands 'windmove-down)

(global-set-key [(control x) (v) (n)] 'git-gutter:next-hunk)
(global-set-key [(control x) (v) (p)] 'git-gutter:previous-hunk)
(global-set-key [(control x) (v) (u)] 'git-gutter:stage-hunk)
(global-set-key [(control x) (v) (R)] 'git-gutter:revert-hunk)
;; (global-set-key [(control x) (v) (r)] 'git-gutter:update-all-windows)
(global-set-key [(control x) (v) (?\s)] 'git-gutter:mark-hunk)

;; (global-set-key (kbd "C-x v =") 'git-gutter:popup-diff)

(global-git-gutter-mode t)

(provide 'git-gutter_init)

;;; git-gutter_init.el ends here
