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
 '(helm-ag-base-command "rg --no-config --no-heading --hidden --smart-case" )
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
 '(helm-ag-ignore-patterns '("!*~" "!#*#" "!*.min.*" "!TAGS" "!tags"))
 )

;; 下面两个一起修改, C-r 搜索 project-root,  因此设为 nil
(setq helm-do-ag--search-this-file-p nil)

(defun helm-quit-and-helm-do-ag-current-directory ()
  "Drop into `helm-do-ag-current-directory' from `helm'."
  (interactive)
  (with-helm-alive-p
    (helm-run-after-exit (lambda () (helm-do-ag default-directory)))
    ))

(global-set-key [(control r)] 'helm-do-ag-this-file)
(define-key helm-ag-map [(control r)] 'helm-quit-and-helm-do-ag-current-directory)

;; Ctrl + l, will up one level, 我为什么要换成 super l? 先撤回。
;; (define-key helm-ag-map [(super l)] 'helm-ag--up-one-level)
;; (define-key helm-do-ag-map [(super l)] 'helm-ag--do-ag-up-one-level)

;; helm-grep-ag-command 设置成 rg 后，用 helm-do-grep-ag

(setq helm-grep-ag-command "rg --no-config --hidden --no-heading --smart-case --color=always --line-number %s %s %s")

(provide 'helm-ag_init)

;;; helm-ag_init.el ends here
