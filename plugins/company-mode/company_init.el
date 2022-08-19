(require 'company)
;; For better performance and results, use company-capf (default)
(require 'company-capf)

(require 'company-dabbrev-code)
(setq company-dabbrev-other-buffers t) ;; 设为 true, 则仅在同一个 major-mode buffer 里面找
(setq company-dabbrev-downcase nil) ;; make dabbrev case-sensitive
;; (setq company-dabbrev-code-ignore-case nil)
;; (setq company-dabbrev-code-everywhere t)

;; FIXME: 测试一下啥效果
;; (setq company-tooltip-limit 5)                      ; bigger popup window
(setq company-tooltip-width-grow-only t) ; 如果 candidates 变宽，tooltip 也跟着变宽，但是不会重新变窄。
(setq company-text-icons-add-background t) ; 生成 icon 的背景

;; FIXME: 测试一下这个
(setq company-begin-commands
      (delete 'c-scope-operator
              (delete 'c-electric-colon
                      (delete 'c-electric-lt-gt
                              (delete 'c-electric-slash
                                      company-begin-commands)))))

;; (setq company-tooltip-align-annotations t) ;; candidate 的注释在 tooltip 右边靠齐
;; (global-set-key (kbd "C-c /") 'company-files)

;; 注意： 默认 TAB 的行为是 company-complete-common, 他会自动完成当前 candidates 的
;; 公共部分， 但是并不会在 candidates 之间移动.
;; company-complete-common-or-cycle 则是：
;; Insert the common part of all candidates, or select the next one.

;; 下面的定义方式是模仿 AC 的行为,
;; 1. 如果 tooltip 出来，TAB 总是选择下一个 candidate， 此时需要回车来确认输入.
;; 2. 如果 tooltip 出来， 但是只有一个结果， TAB 自动输入.
;; 2. 如果 tooltip 没出来，但是有 inline, TAB 自动输入.
;; 这种方案和 yas 完全不冲突。

(defun set-company-tab ()
  ;; (define-key company-active-map [tab] 'company-select-next-if-tooltip-visible-or-complete-selection)
  ;; (define-key company-active-map (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  )

;; (set-company-tab)

;; (global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; 这个可以随时 C-o 随时切换
(define-key company-active-map (kbd "M-/") #'company-complete) ;; 这个和 hippie-expand 等价？
(global-set-key (kbd "C-c C-/") #'company-other-backend)

;; Use M-1,2 ... to select a candidation.
(setq company-show-quick-access t)
(setq company-show-quick-access 'left)

;; 设的稍微短一点，lsp 效果好一些。
(setq company-minimum-prefix-length 2)

;; (setq company-idle-delay
;;       (lambda () (if (company-in-string-or-comment) nil 0.4)))
(setq company-dabbrev-code-everywhere t)

;; 这个其实是替换 company-preview-if-just-one-frontend 为 company-preview-frontend
;; 这样做，会让 preview 总是在光标处 inline 显示。
;; (setq company-frontends
;;       '(company-pseudo-tooltip-unless-just-one-frontend
;;         company-preview-frontend
;;         company-echo-metadata-frontend))

;; 默认
;; (company-bbdb company-cmake company-capf company-files
;;               (company-dabbrev-code company-gtags company-etags company-keywords)
;;               company-dabbrev)

;; 删除一些无用的或可以被 company-capf 替代的 backend
(add-hook 'company-mode-hook
          (lambda ()
            (setq company-backends
                  ;; 根据文档，company-semantic 以及 company-etags 现在都属于 company-capf
                  ;; company-semantic 要开启 semantic-mode 来支持，但是因为只支持有限的语言，因此不用打开。
                  (delete 'company-semantic
                          (delete 'company-oddmuse
                                  (delete 'company-files
                                          (delete 'company-clang
                                                  company-backends)))))
            ))

;; 对于一些简单的模式，组合 company-capf 和 company-ddbbrev-code 会带来大部分期望的结果。
(dolist (hook '(
                graphql-mode-hook
                yaml-mode-hook
                conf-mode-hook
                sh-mode-hook
                crystal-mode
                ;; mint-mode
                ))
  (add-hook hook
            (lambda ()
              (set (make-local-variable 'company-backends) '((company-capf company-dabbrev-code company-keywords)))
              )))

(setq company-files-exclusions '(".git/" ".DS_Store"))


;; (setq company-dabbrev-minimum-length 4)

;; C-n, C-s 如果可以自动打断 tooltip, 其实效果不错。
;; (define-key company-active-map (kbd "C-n") 'company-select-next)
;; (define-key company-active-map (kbd "C-p") 'company-select-previous)

(with-eval-after-load 'yasnippet
  (defun advice-only-show-tooltip-when-invoked (orig-fun command)
    "原始的 company-pseudo-tooltip-unless-just-one-frontend-with-delay, 它一直会显示
candidates tooltip, 除非只有一个候选结果时，此时，它会不显示, 这个 advice 则是让其
完全不显示, 但是同时仍旧保持 inline 提示, 类似于 auto-complete 当中, 设定
ac-auto-show-menu 为 nil 的情形, 这种模式比较适合在 yasnippet 正在 expanding 时使用。"
    (when (company-explicit-action-p)
      (apply orig-fun command)))

  (defun advice-always-trigger-yas (orig-fun &rest command)
    (interactive)
    (unless (ignore-errors (yas-expand))
      (apply orig-fun command)))

  (defun yas/disable-company-tooltip ()
    (interactive)
    (advice-add #'company-pseudo-tooltip-unless-just-one-frontend :around #'advice-only-show-tooltip-when-invoked)
    (define-key company-active-map [tab] 'yas-next-field-or-maybe-expand)
    (define-key company-active-map (kbd "TAB") 'yas-next-field-or-maybe-expand)
    )
  (defun yas/restore-company-tooltip ()
    (interactive)
    (advice-remove #'company-pseudo-tooltip-unless-just-one-frontend #'advice-only-show-tooltip-when-invoked)
    (set-company-tab)
    )
  (add-hook 'yas-before-expand-snippet-hook 'yas/disable-company-tooltip)
  (add-hook 'yas-after-exit-snippet-hook 'yas/restore-company-tooltip)

  ;; 这个可以确保，如果当前 key 是一个 snippet, 则一定展开 snippet,
  ;; 而忽略掉正常的 company 完成。
  (advice-add #'company-select-next-if-tooltip-visible-or-complete-selection :around #'advice-always-trigger-yas)
  (advice-add #'company-complete-common :around #'advice-always-trigger-yas)
  (advice-add #'company-complete-common-or-cycle :around #'advice-always-trigger-yas)
  )

;; 这个开启，lsp 不工作
;; (setq company-insertion-on-trigger t)

;; ;; 注意： C-x = 用来检测光标下字符的数字，(insert 数字) 用来测试数字对应的字符。
;; ;; 32 空格, 41 右圆括号, 46 是 dot 字符
;; ;; 这里我们移除空格，添加逗号(44), 分号(59)
;; (setq company-insertion-triggers '(41 46))

;; (add-hook 'after-init-hook 'global-company-mode)
(global-company-mode 1)

;; (defun ora-company-number ()
;;   "Forward to `company-complete-number'.
;; Unless the number is potentially part of the candidate.
;; In that case, insert the number."
;;   (interactive)
;;   (let* ((k (this-command-keys))
;;          (re (concat "^" company-prefix k)))
;;     (if (or (cl-find-if (lambda (s) (string-match re s))
;;                         company-candidates)
;;             (> (string-to-number k)
;;                (length company-candidates))
;;             (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
;;         (self-insert-command 1)
;;       (company-complete-number
;;        (if (equal k "0")
;;            10
;;          (string-to-number k))))))

;; (defun ora--company-good-prefix-p (orig-fn prefix)
;;   (unless (and (stringp prefix) (string-match-p "\\`[0-9]+\\'" prefix))
;;     (funcall orig-fn prefix)))
;; (advice-add 'company--good-prefix-p :around #'ora--company-good-prefix-p) ;

;; (let ((map company-active-map))
;;   (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
;;         (number-sequence 0 9))
;;   (define-key map " " (lambda ()
;;                         (interactive)
;;                         (company-abort)
;;                         (self-insert-command 1))))

;; (require 'company-box)
;; (with-eval-after-load 'company-box
;;   (add-hook 'company-mode-hook 'company-box-mode))

;; 注意: 这个和 tabnine 冲突.
;; (require 'company-tng)
;; (add-hook 'after-init-hook 'company-tng-mode)

;; (require 'company-fuzzy)
;; (global-company-fuzzy-mode 1)

;; (require 'company-posframe)
;; (with-eval-after-load 'company-posframe
;;   (company-posframe-mode 1)
;;   (require 'desktop) ;this line is needed.
;;   (push '(company-posframe-mode . nil)
;;         desktop-minor-mode-table)
;;   )

;; (require 'company-tabnine_init)

;; toggle-company-english-helper 来开启英文自动补全。
;; 包含了一个 py 脚本，用来转化 stardict 的词库，模式是 KDict, 包含 11 万单词.
;; Execute command `toggle-company-english-helper' to write english on the fly!
(require 'company-english-helper)

(with-eval-after-load 'web-mode
  (require 'company-web-html)
  (add-hook 'web-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '((company-capf company-web-html company-dabbrev-code company-keywords)))
              (company-mode t))
            )
  )

(provide 'company_init)

;;; company_init.el ends here
