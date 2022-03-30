;; edit 用法：
;; helm-git-grep 出来结果后, F3, 直接编辑，完成后 C-x C-s.（或 C-c C-c)
(require 'helm-git-grep)

;; hack for wgrep edit directly use f3
(defun helm-git-grep-save-results-1 ()
  "Save helm git grep result in a `helm-git-grep-mode' buffer."
  (let ((buf "*helm-git-grep*")
        (default-dir (helm-git-grep-base-directory)))
    (with-current-buffer (get-buffer-create buf)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "-*- mode: grep; default-directory: \"%s\" -*-\n\n"
                        default-dir)
                (format "Git Grep Results by: git %s\n\n"
                        (helm-git-grep-concat-string-list
                         (helm-git-grep-args))))
        (save-excursion
          (insert (with-current-buffer helm-buffer
                    (goto-char (point-min)) (forward-line 1)
                    (buffer-substring (point) (point-max)))))
        (setq default-directory default-dir)
        (helm-git-grep-mode)
        (if (fboundp 'wgrep-change-to-wgrep-mode)
            (wgrep-change-to-wgrep-mode))
        (pop-to-buffer buf)))
    (message "Helm Git Grep Results saved in `%s' buffer" buf)))

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

;; 这个使用 helm-do-ag-project-root 替换，因为 helm-git-grep 的 pattern 不知道如何输入空格
;; (global-set-key (kbd "M-r") 'helm-git-grep-at-point-or-helm-do-ag)

(define-key isearch-mode-map (kbd "M-r") 'helm-git-grep-from-isearch)
(define-key helm-map (kbd "M-r") 'helm-git-grep-from-helm)

(provide 'helm-git-grep_init)

;;; helm-git-grep_init.el ends here
