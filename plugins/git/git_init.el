;; -*- lexical-binding: t; -*-

;; C-x v b 查看分支
;; C-x v l 查看 log
;; C-x v g, vc-annoate, 类似于 mo-git-blame, 但是有 blamer 只有, 似乎都不在需要了
;; C-x v a,  尝试交互的使用 ediff 工具来 git add 内容, 基本上不怎么用
(require 'git-emacs_init)

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
