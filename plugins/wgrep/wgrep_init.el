;; wgrep 用来支持编辑 helm-grep 的结果的。
;; 用法：
;; helm-git-grep 出来结果后，按下 TAB, F3, C-c C-p, 然后编辑，完成后 C-x C-s.（或 C-c C-c)

(require 'wgrep)
(require 'wgrep-helm)

(setq wgrep-auto-save-buffer t)

(define-key wgrep-mode-map (kbd "C-c C-c") #'wgrep-finish-edit)

(provide 'wgrep_init)

;;; wgrep_init.el ends here
