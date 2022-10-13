;; wgrep 用来支持编辑 helm-grep 的结果的。
;; 用法：
;; helm-git-grep 出来结果后，按下 F3, C-c C-p, 然后编辑，完成后 C-x C-s.（或 C-c C-c)

;; 或者, 通过 helm 任意工具进入 helm-grep 界面后，在 readonly buffer 里按下 r 开始编辑。
(require 'wgrep)
(require 'wgrep-helm)

(setq wgrep-auto-save-buffer t)

(define-key wgrep-mode-map (kbd "C-c C-c") #'wgrep-finish-edit)

(provide 'wgrep_init)

;;; wgrep_init.el ends here
