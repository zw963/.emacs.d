(require 'git-emacs)
(require 'git-log)
(require 'git-status)

(setq git-working-dir-change-behaviour 'git-refresh-all-saved)

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

(global-set-key [(control x) (v) (b)] 'git-branch) ;; 在 Emacs 下面直接切换分支!
(global-set-key [(control x) (v) (l)] 'git-log-files)   ;; 显示 git 日志窗口.
(global-set-key [(control x) (v) (a)] 'git-add-interactively)         ; git add -i with ediff.

(provide 'git-emacs_init)
;;; git-emacs_init.el ends here
