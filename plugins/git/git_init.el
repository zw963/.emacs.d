;; -*- lexical-binding: t; -*-

;; C-x v b 查看分支
;; C-x v l vc-print-log, 查看日志。
;; C-x v g, vc-annoate, 类似于 mo-git-blame。
(require 'my-git-branch-view-mode)

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

(require 'git-timemachine_init)

;; C-x v * 从第一个存在 diff 的 hunk 开始, 往下查看, 然后可以 n/p 来前进或后退 r 可以 revert, c 拷贝原始内容
(require 'diff-hl_init)
;; (require 'git-gutter_init)

;; (require 'blamer_init)

(require 'git-modes_init)

;; C-x v c 拷贝当前代码在远程的 git 资源所在的行
(require 'git-link_init)

(provide 'git_init)
;;; git_init.el ends here

