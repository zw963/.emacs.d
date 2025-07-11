;; -*- lexical-binding: t; -*-

;; 注意，可以随时通过 C-c C-f 关闭 follow-mode, 但是这个修改是全局的，稍后还要再改回来。
(require 'helm-common_init)
(require 'helm-ag_init)

(setq helm-source-names-using-follow '("Git-Grep"))

;; 如果 helm-git-grep 开启，会覆盖 helm-ag_init 的 M-R 快捷键
;; (require 'helm-git-grep_init)
(require 'helm-grep_init)

;; (global-set-key (kbd "M-r") 'helm-grep-do-git-grep)

(require 'helm-ls-git_init)
;; (require 'helm-fasd)
;; (require 'helm-rg)

(require 'helm-flx)
(setq helm-flx-for-helm-find-files t ;; t by default
      helm-flx-for-helm-locate t) ;; nil by default
(helm-flx-mode 1)

(require 'helm-descbinds)
(helm-descbinds-mode)

(provide 'helm_init)
;;; helm_init.el ends here
