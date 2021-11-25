;; edit 用法：
;; helm-git-grep 出来结果后, F3, 直接编辑，完成后 C-x C-s.（或 C-c C-c)
(require 'helm-git-grep)

(setq helm-git-grep-pathspecs '("*" ":!:*.min.js*" ":!:*.less" ":!:*/vendor/assets*"))
(setq helm-git-grep-ignore-case nil)

(setq helm-git-grep-source
      (helm-make-source "Git Grep" 'helm-git-grep-class
        :candidates-process 'helm-git-grep-process
        :follow (and helm-follow-mode-persistent 1)))

(setq helm-git-grep-submodule-source
      (helm-make-source "Git Submodule Grep" 'helm-git-grep-class
        :candidates-process 'helm-git-grep-submodule-grep-process
        :follow (and helm-follow-mode-persistent 1)))

(defun helm-git-grep-at-point-or-helm-do-ag ()
  (interactive)
  (if (and (fboundp 'helm-ls-git-root-dir) (helm-ls-git-root-dir))
      (call-interactively 'helm-git-grep-at-point)
    (call-interactively 'helm-do-ag-this-file)))

(global-set-key (kbd "M-r") 'helm-git-grep-at-point-or-helm-do-ag)
(define-key isearch-mode-map (kbd "M-r") 'helm-git-grep-from-isearch)
(define-key helm-map (kbd "M-r") 'helm-git-grep-from-helm)

(provide 'helm-git-grep_init)

;;; helm-git-grep_init.el ends here
