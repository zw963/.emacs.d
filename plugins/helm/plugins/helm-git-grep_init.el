;; wgrep 用来支持编辑 grep 结果的。
;; 用法：
;; helm-git-grep 出来结果后，按下 TAB, F3, C-c C-p, 然后编辑，完成后 C-c C-c.

;; (require 'wgrep)
;; (require 'wgrep-helm)

(require 'helm-git-grep)

(setq helm-git-grep-pathspecs '("*" ":!:*.min.js*" ":!:*.less" ":!:*/vendor/assets*"))

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
    (call-interactively 'helm-do-ag)))

;; (push '("Save results in grep buffer" . helm-git-grep-save-results)
;;       helm-git-grep-actions)
;; (delete-dups helm-git-grep-actions)

(global-set-key (kbd "M-r") 'helm-git-grep-at-point-or-helm-do-ag)
(define-key isearch-mode-map (kbd "M-r") 'helm-git-grep-from-isearch)
(define-key helm-map (kbd "M-r") 'helm-git-grep-from-helm)

(provide 'helm-git-grep_init)

;;; helm-git-grep_init.el ends here
