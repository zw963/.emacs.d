;; 记住, neotree 的默认快捷键都有个 C-c 前缀.
;; 如果忘记了, C-k C-m 查询
(require 'neotree)

;; 默认 neo-hidden-regexp-list 包含 . 开头的隐藏文件.
;; 快捷键 H 可以切换是否显示隐藏文件
(add-list-to-list 'neo-hidden-regexp-list
                  '(
                    "\\.git" "\\.idea" "TAGS"
                    "vendor" "fixtures" "tmp"
                    "log" "^\\.#" "\\.bundle"
                    "\\.lock" "node_modules"
                    "coverage" "public" "railsbox"
                    "\\.min\\.*"
                    ))

(setq neo-theme 'icons)
(setq neo-window-width 40)
(setq neo-auto-indent-point t)
(setq neo-window-fixed-size nil)
(setq neo-show-slash-for-folder nil)
(setq neo-show-updir-line nil)
;; (setq neo-hide-cursor t)
(setq neo-smart-open t)                 ; 开启 neotree 窗口时, 尝试跳转到当前文件 node
;; (setq neo-autorefresh t)             ; 开启速度很慢.
;; (setq neo-vc-integration t)
;; (setq neo-confirm-change-root 'noop)
(global-set-key [(f8)] 'neotree-toggle)

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map [(f8)] 'neotree-toggle)
             ))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (define-key ibuffer-mode-map [(f8)] 'neotree-toggle)
             ))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (define-key ibuffer-mode-map [(f8)] 'neotree-toggle)
             ))

;; 下面的定义也包涵默认的定义.(备忘)
(define-key neotree-mode-map [(D)] 'neotree-delete-node)
(define-key neotree-mode-map [(C)] 'neotree-copy-node)
(define-key neotree-mode-map [(R)] 'neotree-rename-node)
(define-key neotree-mode-map [(U)] 'neotree-select-up-node)
(define-key neotree-mode-map [(backspace)] 'neotree-select-up-node)
(define-key neotree-mode-map [(H)] 'neotree-hidden-file-toggle)
;; make neotree very widely.
(define-key neotree-mode-map [(A)] 'neotree-stretch-toggle)
(define-key neotree-mode-map [(q)] 'neotree-hide)
(define-key neotree-mode-map [(control c) (control a)] 'neotree-collapse-all)
(define-key neotree-mode-map [(control c) (control c)] 'neotree-change-root)

;; 下面的快捷键工作, 但是无法自定义.
;; (define-key neotree-mode-map [(a)] 'neo-open-file-ace-window)
;; (define-key neotree-mode-map [(O)] 'neo-open-dir-recursive)
;; (define-key neotree-mode-map (kbd "|") 'neo-open-file-vertical-split)
;; (define-key neotree-mode-map (kbd "-") 'neo-open-file-horizontal-split)

(provide 'neotree_init)
;;;  neotree_init.el ends here
