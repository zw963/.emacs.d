(require 'rime)

(rime-mode 1)

(setq default-input-method "rime")

;; 默認配置文件存儲位置： ~/.emacs.d/rime/
(setq rime-user-data-dir `,(concat default-directory "rime"))

(setq rime-show-candidate 'posframe)

(defun +rime--posframe-display-content-a (args)
  "给 `rime--posframe-display-content' 传入的字符串加一个全角空
格，以解决 `posframe' 偶尔吃字的问题。"
  (cl-destructuring-bind (content) args
    (let ((newresult (if (string-blank-p content)
                         content
                       (concat content "　"))))
      (list newresult))))

;; (global-set-key (kbd "C-SPC") 'toggle-input-method)

;; 通过 xremap 配置，如果当前是 Emacs 窗口的话，单击 Capslock 将会映射为 F8
;; 因此绑定到 F8，默认是 C-\, 已经在操作系统全局绑定为其他命令了。
(global-set-key [(f8)] 'toggle-input-method)

(if (fboundp 'rime--posframe-display-content)
    (advice-add 'rime--posframe-display-content
                :filter-args
                #'+rime--posframe-display-content-a)
  (error "Function `rime--posframe-display-content' is not available."))

;; (defvar sis--for-buffer nil
;;  "Saved buffer input source.")
;; (make-variable-buffer-local 'sis--for-buffer)

;; (add-hook input-method-activate-hook (lambda ()
;;                                       (setq-local sis--for-buffer t)
;;                                       ))
;; (add-hook input-method-deactivate-hook (lambda ()
;;                                       (setq-local sis--for-buffer nil)
;;                                       ))

(defvar input-method-cursor-color "Orange"
  "Default cursor color if using an input method.")

(defvar default-cursor-color (frame-parameter nil 'cursor-color)
  "Default text cursor color.")

(defun change-cursor-color-on-input-method ()
  "Set cursor color depending on whether an input method is used or not."
  (interactive)
  (set-cursor-color (if current-input-method
                        input-method-cursor-color
                      default-cursor-color)))

(add-hook 'post-command-hook 'change-cursor-color-on-input-method)

;; (setq rime-posframe-properties
;;       (list :background-color "#333333"
;;             :foreground-color "#dcdccc"
;;             :font "yaheiInconsolata-14"
;;             :internal-border-width 10))

(setq mode-line-mule-info '((:eval (rime-lighter))))

;; 这个用来切换繁简，这个和运行 shell 冲突
(global-set-key (kbd "C-`") 'rime-send-keybinding)

(setq rime-inline-predicates '(
                               ;; rime-predicate-space-after-cc-p
                               rime-predicate-current-uppercase-letter-p
                               ;; rime-predicate-prog-in-code-p
                               rime-predicate-ace-window-p
                               ;; rime-predicate-prog-in-code-p
                               ;; rime-predicate-org-in-src-block-p
                               ;; rime-predicate-in-code-string-after-ascii-p
                               )
      )

(provide 'rime_init)

;;; rime_init.el ends here
