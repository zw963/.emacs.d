;; ============================================================================
;;
;; 全局快捷键绑定
;;
;; ============================================================================

;; 添加几个需要的前缀。
(define-prefix-command 'meta-c-map)
(global-set-key [(meta c)] 'meta-c-map)

(global-set-key [(control c) (j)] 'imenu)
(global-set-key [(super n)] 'window-move-up) ;光标位置不变，窗口向上移动四行
(global-set-key [(super p)] 'window-move-down) ;光标位置不变，窗口向下移动两行
(global-set-key [(meta P)] 'other-window-move-down) ;下一个窗口向下移动两行
(global-set-key [(meta N)] 'other-window-move-up) ;下一个窗口向上移动一行
(global-set-key [(meta \`)] 'other-frame) ; 如果默认切换无法工作，使用 Emacs 内置的 other-frame 功能。
(global-set-key [(meta \~)] '(lambda () (interactive) (other-frame -1)))
(global-set-key [(f2)] 'kill-buffer-enhanced) ;关闭当前缓冲区 F2
(global-set-key [(control x) (\2)] 'split-window-below-then-switch-to)
(global-set-key [(control x) (\3)] 'split-window-right-then-switch-to)
;; (global-set-key [(control \8)] 'delete-window)
(global-set-key [(control \8)] 'keyboard-escape-quit) ;当前窗口最大化
(global-set-key [remap dabbrev-expand] 'hippie-expand) ;;替换默认自动补全为hippie-expand
(global-set-key [(meta D)] '(lambda () (interactive) (dired "./"))) ; 打开 dired buffer.
(global-set-key [(control right)] 'transpose-current-char) ;光标前所在字母右移
(global-set-key [(control left)] 'transpose-current-char-backward) ;光标前所在字母左移
(global-set-key [(del)] 'delete-char) ;向后删除字符 Del
(global-set-key [(control meta c)] 'copy-current-buffer-file-name)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(control c) (control o)] 'browse-url-at-point) ;C-c C-o 自动打开链接.
(global-set-key [(meta \=)] 'ediff-windows-linewise) ;使用ediff逐行对比buffer内容
(global-set-key [(shift f2)] 'rename-current-buffer-file)
(global-set-key [(control ?\.)] 'input-rocket-with-space)
(global-set-key [(control s)] 'isearch-forward-regexp) ;更改Ctrl-s为正则搜索
(global-set-key [(control f11)] 'bc1-current-file) ; Ctrl-F11 bc1
(global-set-key [(meta f11)] 'bc2-current-file) ; Meta-F11 bc2
(global-set-key [(meta t)] 'translate-this-word-or-region)
(global-set-key [(?\,)] 'input-comma-with-space)
(global-set-key '[(backtab)] 'backtab-space)

(global-set-key [(control x) (j)] 'previous-buffer)
(global-set-key [(control x) (l)] 'next-buffer)

(global-set-key [(meta c) (\.)] 'input-comment-with-rocket)
(global-set-key [(meta c) (=)] 'input-add-equal)

(global-set-key [(control c ) (a)] 'align-regexp)

(define-key key-translation-map [(meta n)] [(super n)])
(define-key key-translation-map [(meta p)] [(super p)])
;; (define-key key-translation-map [(control l)] [(super l)])

(add-hook 'compilation-mode-hook
          '(lambda ()
             (define-key compilation-mode-map [(super n)] 'compilation-next-error)
             (define-key compilation-mode-map [(super p)] 'compilation-previous-error)
             (define-key compilation-mode-map [(n)] 'compilation-next-error)
             (define-key compilation-mode-map [(p)] 'compilation-previous-error)
             (define-key compilation-mode-map [(control \8)] 'quit-window)
             ))

;;  在命令行下, 貌似 C-2 默认等价于: C-@，这里使用同样的绑定。
(define-key key-translation-map [(control \2)] [(control \@)])

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(global-set-key [(control backspace)] 'backward-kill-line)
(global-set-key [(control ?\d)] 'backward-kill-line)

;; (define-prefix-command 'control-c-control-c-map)
;; (global-set-key [(control c) (control c)] 'control-c-control-c-map)

(dolist (hook '(prog-mode-hook
                yaml-mode-hook
                elixir-mode-hook
                nxml-mode-hook
                html-mode-hook
                rhtml-mode-hook
                handlebars-mode-hook
                slim-mode-hook
                feature-mode-hook
                conf-mode-hook))
  (add-hook hook '(lambda ()
                    (local-set-key [(control c) (control c)] 'format-buffer)
                    (local-set-key [(?\,)] 'input-comma-with-space)
                    ;; (local-set-key [(?\;)] 'input-semicolon-with-space)
                    (local-set-key [(control return)] 'textmate-next-line)
                    )))

(defun format-buffer ()
  "Perform a bunch of operations of a buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max))
  ;; (unless dont-indent-line
  ;;   )
  )

(defun go-to-line-and-column-cond (lc-cond)
  (interactive "sLine:Column:: ")
  (let (line delim column max-lines)
    (setq max-lines (count-lines (point-min) (point-max)))
    (save-match-data
      (string-match "^\\([0-9]*\\)\\([,:]?\\)\\([0-9]*\\)$" lc-cond)
      (setq line (string-to-number (match-string 1 lc-cond)))
      (setq delim (match-string 2 lc-cond))
      (setq column (string-to-number (match-string 3 lc-cond)))
      (if (not (equal delim "")) (if (> column 0) (setq column (1- column))))
      (if (= 0 line) (setq line (line-number-at-pos)))
      (if (> line max-lines) (setq line max-lines))
      (goto-line line)
      (move-to-column column)
      (message "Marker set to line %d column %s" (line-number-at-pos) (current-column))
      )))
(global-set-key [remap goto-line] 'go-to-line-and-column-cond)

(defun input-semicolon-with-space ()
  (interactive)
  (if (fourth (syntax-ppss))
      (insert ";")
    (if (not (looking-back "; " (line-beginning-position)))
        (insert "; ")
      (progn
        (backward-delete-char 1)
        (insert ";")))))

(defun textmate-next-line ()
  "Inserts an indented newline after the current line and moves the point to it."
  (interactive)
  (end-of-line)
  (cond
   ((member major-mode '(rust-mode rustic-mode)) (insert ";")
    ))
  ;; (electric-newline-and-maybe-indent)
  ;; (newline-and-indent)

  (if (fifth (syntax-ppss))
      ;; 记住: 总可以通过Ctrl+Alt+j 执行下面的功能.(注释中新行自动注释)
      (default-indent-new-line)
    (reindent-then-newline-and-indent)))

(defun input-comma-with-space ()
  (interactive)
  (if (or
       ;; (fourth (syntax-ppss))
       ;; (member (fourth (syntax-ppss)) (list ?\/))
       (and (member major-mode '(sh-mode
                                 conf-space-mode
                                 conf-unix-mode
                                 emacs-lisp-mode
                                 lisp-interaction-mode
                                 snippet-mode))
            ;; (fifth (syntax-ppss)) is comment
            (not (fifth (syntax-ppss)))))
      (insert ",")
    (insert ", ")))

(defun nxml-mark-sexp-tag (&optional arg)
  (interactive "p")
  (push-mark nil t t)
  (nxml-forward-element arg))

(add-hook 'nxml-mode-hook
          '(lambda ()
             (local-set-key [(control meta ?\s)] 'nxml-mark-sexp-tag)
             (local-set-key [(control meta f)] 'nxml-forward-element)
             (local-set-key [(control meta b)] 'nxml-backward-element)
             ))

(dolist (hook '(apropos-mode-hook
                help-mode-hook
                speedbar-mode-hook
                yari-mode-hook
                compilation-shell-minor-mode-hook))
  (add-hook hook '(lambda ()
                    (local-set-key [(control \8)] 'delete-window)
                    )))

(add-hook 'text-mode-hook (lambda ()
                            (local-set-key [(control c) (return)] 'org-ctrl-c-ret)
                            (local-set-key [(control c) (?\r)] 'org-ctrl-c-ret)
                            ;; (local-set-key [(meta return)] 'org-return-indent)
                            ))

(define-key indent-rigidly-map [(control b)] 'indent-rigidly-left)
(define-key indent-rigidly-map [(control f)] 'indent-rigidly-right)
(define-key indent-rigidly-map [(meta b)]  'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map [(meta f)] 'indent-rigidly-right-to-tab-stop)

(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (define-key minibuffer-local-map [(super n)] 'next-history-element)
             (define-key minibuffer-local-map [(super p)] 'previous-history-element)
             (define-key minibuffer-local-map "\M-n" nil)
             (define-key minibuffer-local-map "\M-p" nil)
             (define-key minibuffer-local-map "\C-n" nil)
             (define-key minibuffer-local-map "\C-p" nil)
             ))

(add-hook 'isearch-mode-hook
          '(lambda ()
             (define-key isearch-mode-map [(control b)] 'isearch-delete-char)
             (define-key isearch-mode-map [(control f)] 'isearch-yank-char)
             (define-key isearch-mode-map [(meta f)] 'isearch-yank-word)
             (define-key isearch-mode-map [(control e)] 'isearch-yank-line)
             (define-key isearch-mode-map [(control y)] 'isearch-yank-kill)
             (define-key isearch-mode-map [(meta \')] 'expand-abbrev)
             (define-key isearch-mode-map [(super n)] 'isearch-repeat-forward)
             (define-key isearch-mode-map [(super p)] 'isearch-repeat-backward)
             (define-key isearch-mode-map [(meta \5)] 'isearch-query-replace-regexp)
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
