;; ============================================================================
;;
;; 全局快捷键绑定
;;
;; ============================================================================

;; (define-key key-translation-map [(meta h)] [(super h)])
;; (define-key key-translation-map [(meta l)] [(super l)])
;; (define-key key-translation-map [(meta k)] [(super k)])
;; (define-key key-translation-map [(meta j)] [(super j)])

;; (require 'windmove)
;; (global-set-key [(super h)] 'windmove-left)
;; (global-set-key [(super l)] 'windmove-right)
;; (global-set-key [(super k)] 'windmove-up)
;; (global-set-key [(super j)] 'windmove-down)

(global-set-key [(meta \`)] 'other-frame) ; 如果默认切换无法工作，使用 Emacs 内置的 other-frame 功能。
(global-set-key [(meta \~)] (lambda () (interactive) (other-frame -1)))
;; (global-set-key [(control \8)] 'delete-window)
(global-set-key [(control \8)] 'keyboard-escape-quit) ;当前窗口最大化
(global-set-key [remap dabbrev-expand] 'hippie-expand) ;;替换默认自动补全为hippie-expand
;; 重构时，关闭的快捷键
;; (global-set-key [(del)] 'delete-char) ;向后删除字符 Del
;; (global-set-key [(meta g)] 'goto-line)

(global-set-key [(control c) (control o)] 'browse-url-at-point) ;C-c C-o 自动打开链接.
(global-set-key [(meta \=)] 'ediff-windows-linewise) ;使用ediff逐行对比buffer内容
(global-set-key [(control x) (j)] 'previous-buffer)
(global-set-key [(control x) (l)] 'next-buffer)

(global-set-key [(control c ) (a)] 'align-regexp)

;; (define-key key-translation-map [(control l)] [(super l)])

;; (defun backward-kill-line (arg)
;;   "Kill ARG lines backward."
;;   (interactive "p")
;;   (kill-line (- 1 arg)))

;; (global-set-key [(control backspace)] 'backward-kill-line)
;; (global-set-key [(control ?\d)] 'backward-kill-line)

;; (define-prefix-command 'control-c-control-c-map)
;; (global-set-key [(control c) (control c)] 'control-c-control-c-map)

(defun nxml-mark-sexp-tag (&optional arg)
  (interactive "p")
  (push-mark nil t t)
  (nxml-forward-element arg))

(add-hook 'nxml-mode-hook
          (lambda ()
            (local-set-key [(control meta ?\s)] 'nxml-mark-sexp-tag)
            (local-set-key [(control meta f)] 'nxml-forward-element)
            (local-set-key [(control meta b)] 'nxml-backward-element)
            ))

(dolist (hook '(apropos-mode-hook
                help-mode-hook
                speedbar-mode-hook
                yari-mode-hook
                compilation-shell-minor-mode-hook))
  (add-hook hook (lambda ()
                   (local-set-key [(control \8)] 'delete-window)
                   )))

(add-hook 'text-mode-hook (lambda ()
                            (local-set-key [(control c) (return)] 'org-ctrl-c-ret)
                            (local-set-key [(control c) (?\r)] 'org-ctrl-c-ret)
                            ;; (local-set-key [(meta return)] 'org-return-indent)
                            ))

;; ------------------------------ 关闭的按键绑定 ------------------------------
;; move char
;; (define-key key-translation-map [(control j)] [(control b)])
;; (define-key key-translation-map [(shift control j)] [(shift control b)])
;; (define-key key-translation-map [(control l)] [(control f)])
;; (define-key key-translation-map [(shift control l)] [(shift control f)])
;; (define-key key-translation-map [(control x) (control j)] [(control x) (control j)])
;; (define-key key-translation-map [(control x) (control l)] [(control x) (control l)])
;; (define-key key-translation-map [(control c) (control j)] [(control c) (control j)])
;; (define-key key-translation-map [(control c) (control l)] [(control c) (control l)])

;; move word
;; (define-key key-translation-map [(meta j)] [(meta b)])
;; (define-key key-translation-map [(shift meta j)] [(shift meta b)])
;; (define-key key-translation-map [(meta l)] [(meta f)])
;; (define-key key-translation-map [(shift meta l)] [(shift meta f)])
;; move sexp
;; (define-key key-translation-map [(control meta j)] [(control meta b)])
;; (define-key key-translation-map [(control meta l)] [(control meta f)])
;; page
;; (define-key key-translation-map [(meta v)] [(control v)])
;; (define-key key-translation-map [(meta q)] [(meta v)])
;; just for old `control l' `control j'
;; (define-key key-translation-map [(control z)] [(control l)])
;; (define-key key-translation-map [(meta z)] [(control j)])
;; set mark
;; (define-key key-translation-map [(meta \1)] [(control a)])
;; (define-key key-translation-map [(meta \2)] [(control ?\s)])
;; (define-key key-translation-map [(meta \3)] [(control e)])
