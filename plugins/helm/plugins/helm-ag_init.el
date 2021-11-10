;; C-c C-e 开启编辑模式，配合 iedit-mode, 可以批量替换内容。非常酷！

;; helm-ag-ignore-buffer-patterns
(require 'helm-ag)

;; Hack for ignore pattern works with rg
(defun helm-ag--construct-ignore-option (pattern)
  "Not documented, PATTERN."
  (concat "--glob=" pattern))

(custom-set-variables
 ;; 必须开启 follow 模式, 才能有跟随 buffer 的 follow 效果.
 '(helm-follow-mode-persistent t)
 ;; helm-ag-base-command "ag --nocolor --nogroup --ignore-case"
 ;; '(helm-ag-base-command "rg --hidden --with-filename --no-heading --smart-case")
 '(helm-ag-base-command "rg --no-config --no-heading --hidden --smart-case")
 ;; '(helm-ag-base-command "rg --no-config --no-heading --hidden --smart-case" )
 '(helm-ag-insert-at-point 'symbol)
 '(helm-ag-fuzzy-match t)
 ;; 下面的选项只是适合于 ag, rg 使用
 ;; '(helm-ag-ignore-patterns '(
 ;;                             "*.min.js" "*.js" "*.less"
 ;;                             "*.json" "*.log" "TAGS"
 ;;                             "tags" "test-reports"
 ;;                             "\\#*\\#" ".\\#*" "*~"
 ;;                             ))
 '(helm-ag-ignore-patterns '("!*~" "!#*#" "!*.min.*" "!TAGS" "!tags" "!.git/"))
 )

(setq helm-ag--actions
      '(("Open file" . helm-ag--action-find-file)
        ("Open file other window" . helm-ag--action-find-file-other-window)
        ("Edit search results" . helm-ag--edit)
        ("Save results in buffer" . helm-ag--action-save-buffer))
      )

(setq helm-do-ag-on-current-directory-p nil)
(defun helm-quit-and-helm-do-ag-on-current-directory ()
  "Drop into `helm-do-ag' on DEFAULT-DIRECTORY from `helm'."
  (interactive)
  (setq helm-do-ag-on-current-directory-p t)
  (with-helm-alive-p
    (helm-run-after-exit #'helm-do-ag default-directory nil helm-pattern)))
(defun advice-up-on-level-corretly-when-run-helm-do-ag-this-file (orig-fun &rest command)
  "If start helm-ag with `helm-do-ag-this-file', `helm-ag--do-ag-up-one-level' not work,
we have to run `helm-do-ag' on DEFAULT-DIRECTORY first, then up one level function start to work."
  (if helm-do-ag-on-current-directory-p
      (apply orig-fun command)
      (helm-quit-and-helm-do-ag-on-current-directory)))

(advice-add #'helm-ag--do-ag-up-one-level :around #'advice-up-on-level-corretly-when-run-helm-do-ag-this-file)
(global-set-key [(control r)] 'helm-do-ag-this-file)
(define-key helm-do-ag-map [(control r)] 'helm-ag--do-ag-up-one-level)
(add-hook 'helm-quit-hook (lambda () (setq helm-do-ag-on-current-directory-p nil)))

;; helm-grep-ag-command 设置成 rg 后，用 helm 自带的 helm-do-grep-ag 命令调用下面这个。
'(helm-grep-ag-command "rg --no-config --no-heading --hidden --smart-case --color=always --line-number %s %s %s")

(provide 'helm-ag_init)

;;; helm-ag_init.el ends here
