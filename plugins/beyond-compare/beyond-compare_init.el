;; -*- lexical-binding: t; -*-

;; ============================== Beyond Comapre 集成 ==============================
(require 'beyond-compare-functions)

(global-set-key [(control f11)] 'bc1-current-region)
(global-set-key [(meta f11)] 'bc2-current-region)

(global-set-key [(control f12)] 'bc1-current-file)
(global-set-key [(meta f12)] 'bc2-current-file)

(require 'dired)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map [(control f12)] 'dired-bc1-current-file)
            (define-key dired-mode-map [(meta f12)] 'dired-bc2-current-file)
            ))

(require 'ibuffer)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (define-key ibuffer-mode-map [(control f12)] 'ibuffer-bc1-current-file)
            (define-key ibuffer-mode-map [(meta f12)] 'ibuffer-bc2-current-file)
            ))

(require 'vc-annotate)
(add-hook 'vc-annotate-mode-hook
          (lambda ()
            (define-key vc-annotate-mode-map [(n)] 'next-line)
            (define-key vc-annotate-mode-map [(p)] 'previous-line)
            (define-key vc-annotate-mode-map [(meta g) (meta g)] 'vc-annotate-goto-line)
            (define-key vc-annotate-mode-map [(d)] 'bc3-gd1-file-at-point)
            ))

(require 'vc-git)
(add-hook 'vc-git-log-view-mode-hook
          (lambda ()
            (define-key vc-git-log-view-mode-map [(=)] 'bc3-gitdiff-file-at-point) ;; 当前文件与 commit 之间的修改.
            ))

(with-eval-after-load 'git-emacs
  (global-set-key [(control x) (v) (=)] 'bc3-gitdiff) ;; 当前 diff 修改.
  (global-set-key [(control x) (v) (h)] 'bc3-gitdiff-head) ;; diff + index 修改
  )

(add-hook 'git--branch-mode-hook
          (lambda ()
            (define-key git--branch-mode-map [(=)] 'bc3-gitdiff-file-at-point) ;; 指定分支和 commit 之间的修改.
            ))

(add-hook 'git-log-view-mode-hook
          (lambda ()
            (define-key git-log-view-mode-map [(=)] 'bc3-gitdiff-file-at-point) ;; 当前文件与 commit 之间的修改.
            (define-key git-log-view-mode-map [(d)] 'bc3-gd1-file-at-point) ;; 单个 commit 做了那些修改, 只看当前文件
            (define-key git-log-view-mode-map [(q)] 'kill-buffer-and-window)
            ))

(add-hook 'mo-git-blame-mode-hook
          (lambda ()
            (define-key mo-git-blame-mode-map [(d)] 'bc3-gd1-file-at-point)
            ))

(provide 'beyond-compare_init)

;;; beyond-compare_init.el ends here
