(require 'git-emacs)
(require 'git-log)
(require 'git-status)

(setq git-working-dir-change-behaviour 'git-refresh-all-saved)

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

(define-key vc-git-log-view-mode-map [(=)] 'bc3-gitdiff-file-at-point) ;; 当前文件与 commit 之间的修改.
(define-key git-log-view-mode-map [(=)] 'bc3-gitdiff-file-at-point) ;; 当前文件与 commit 之间的修改.
(define-key git-log-view-mode-map [(d)] 'bc3-gd1-file-at-point) ;; 单个 commit 做了那些修改, 只看当前文件
(define-key git-log-view-mode-map [(q)] 'kill-buffer-and-window)

(define-key git--branch-mode-map [(=)] 'bc3-gitdiff-file-at-point) ;; 指定分支和 commit 之间的修改.

(global-set-key [(control x) (v) (=)] 'bc3-gitdiff) ;; 当前 diff 修改.
(global-set-key [(control x) (v) (h)] 'bc3-gitdiff-head) ;; diff + index 修改
(global-set-key [(control x) (v) (b)] 'git-branch) ;; 在 Emacs 下面直接切换分支!
(global-set-key [(control x) (v) (l)] 'git-log-files)   ;; 显示 git 日志窗口.
(global-set-key [(control x) (v) (a)] 'git-add-interactively)         ; git add -i with ediff.

(defun bc3-gitdiff-file-at-point ()
  "Git diff current file with pointing tag, Use Beyond Compare 3."
  (interactive)

  (cond ((eq major-mode 'git-branch-mode)
         (run-process "git-bcompare"
                      (thing-at-point 'symbol)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ((eq major-mode 'git-log-view-mode)
         (run-process "git-bcompare"
                      (log-view-current-tag)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ((eq major-mode 'git-state-mode)
         (let ((fn (git--status-view-select-filename))
               (stat (git--fileinfo->stat (ewoc-data (ewoc-locate git--status-view)))))
           (if (equal stat 'staged)
               ;; INDEX 内将要提交的改变.
               (run-process "git-bcompare" "--cached" "--" fn)
             ;; 提交的改变.
             (run-process "git-bcompare" "--" fn))))
        ))

(defun bc3-gd1-file-at-point ()
  "git gd1 current file with pointing tag, Use Beyond Compare 3."
  (interactive)
  (cond ((eq major-mode 'mo-git-blame-mode)
         (run-process "gd1"
                      (thing-at-point 'symbol)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))
                      ))
        ((eq major-mode 'git-log-view-mode)
         (run-process "gd1"
                      (log-view-current-tag)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ((eq major-mode 'vc-annotate-mode)
         (run-process "gd1"
                      (car (vc-annotate-extract-revision-at-line))
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ))

(defun bc3-gitdiff-head ()
  "Git diff current buffer with HEAD, Use Beyond Compare 3."
  (interactive)
  (git--require-buffer-in-git)
  (run-process "git-bcompare" "HEAD" "--" buffer-file-name))

(defun bc3-gitdiff ()
  "Git diff current file content with INDEX, Use Beyond Compare 3."
  (interactive)
  (git--require-buffer-in-git)
  (run-process "git-bcompare" "--" (file-relative-name buffer-file-name)))

(require 'vc-annotate)

(add-hook 'vc-annotate-mode-hook
          (lambda ()
            (define-key vc-annotate-mode-map [(n)] 'next-line)
            (define-key vc-annotate-mode-map [(p)] 'previous-line)
            (define-key vc-annotate-mode-map [(meta g)] 'vc-annotate-goto-line)
            (define-key vc-annotate-mode-map [(d)] 'bc3-gd1-file-at-point)
            ))

(provide 'git-emacs_init)
;;; git-emacs_init.el ends here
