(require 'company)
;; For better performance and results, use company-capf (default)
(require 'company-capf)

;; FIXME: 下面的是什么意思?
;; (setq company-tooltip-align-annotations t)

;; (setq company-tooltip-width-grow-only t)
;; (setq company-dabbrev-other-buffers nil)
;; ;; make dabbrev case-sensitive
;; (setq company-dabbrev-ignore-case nil)
;; (setq company-dabbrev-downcase nil)
;; (setq company-dabbrev-code-ignore-case nil)
;; (setq company-dabbrev-code-everywhere t)

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
  (define-key company-active-map [tab] 'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection)
  )

(set-company-tab)

(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
(define-key company-active-map (kbd "S-TAB") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)

;; Use M-1,2 ... to select a candidation.
(setq company-show-quick-access t)

;; 这个其实是替换 company-preview-if-just-one-frontend 为 company-preview-frontend
;; 这样做，会让 preview 总是在光标处 inline 显示。
(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))

;; company-backends 工厂默认值为
;; (company-bbdb company-semantic company-cmake company-capf company-clang company-files
;;            (company-dabbrev-code company-gtags company-etags company-keywords)
;;            company-oddmuse company-dabbrev)

(add-hook 'company-mode-hook (lambda ()
                               (setq company-backends
                                     (delete 'company-oddmuse
                                             (delete 'company-bbdb
                                                     (delete 'company-cmake
                                                             (delete 'company-clang
                                                                     (delete '(company-dabbrev-code company-gtags company-etags company-keywords) company-backends))))))
                               ))

(setq company-dabbrev-code-everywhere t)

(dolist (hook '(sh-mode-hook graphql-mode-hook))
  (add-hook hook (lambda ()
                   (add-to-list (make-local-variable 'company-backends) '(company-dabbrev-code company-keywords))
                   )))

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

(setq company-auto-commit nil)
;; 32 空格, 41 右圆括号, 46 是 dot 字符
;; 这里我们移除空格，添加逗号(44), 分号(59)
;; 注意： C-x = 用来检测光标下字符的数字，(insert 数字) 用来测试数字对应的字符。
(setq company-auto-commit-chars '(41 46))
;; (setq company-require-match nil)

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

(require 'company-tabnine_init)

(provide 'company_init)

;;; company_init.el ends here
