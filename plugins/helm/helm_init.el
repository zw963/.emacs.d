(require 'helm-icons)
(helm-icons-enable)

;; 注意，可以随时通过 C-c C-f 关闭 follow-mode, 但是这个修改是全局的，稍后还要再改回来。
(require 'helm-common_init)
(require 'helm-ag_init)
;; TODO: use helm-rg replace helm-ag
(require 'helm-git-grep_init)
(require 'helm-ls-git_init)
(require 'helm-fasd)
;; (require 'popwin_init)

(provide 'helm_init)
;;; helm_init.el ends here
