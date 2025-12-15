;; 不要把 rect 相关的函数和 multli-cursor 一起使用，那个是批量修改的，而不是批量复制粘贴的。

;; 开启 rectangle-mark-mode, 快捷键: C-x SPC
;; C-x r y, yank-rectangle 粘帖选区内容
;; C-x r k, kill-rectangle 剪切选区内容
;; C-x r M-w, copy-rectangle-as-kill 复制选区内容
;; C-x r d, delete-rectangle 删除选区, 文字左移
;; C-x r N, rectangle-number-lines  选区前面插入数字
;; C-x r t, string-rectangle 在指定的选区之前添加内容.

(require 'rect)
(require 'second-sel)

(defun convert-to-secondary-region ()
  (interactive)
  (primary-to-secondary (region-beginning) (region-end))
  (delete-overlay mouse-secondary-overlay)
  (deactivate-mark))

;; 下面两套 device 都是针对 kill-ring-save, kill-region。
(advice-add 'kill-ring-save :around
            (lambda (orig-fn &rest args)
              "Let 'kill-ring-save support secondary ring."
              (when (use-region-p) (convert-to-secondary-region))
              (apply orig-fn args)))

(advice-add 'kill-region :around
            (lambda (orig-fn &rest args)
              "Let 'kill-region support secondary ring."
              (when (use-region-p) (convert-to-secondary-region))
              (apply orig-fn args)))

(global-set-key [(control meta y)] 'yank-secondary) ; C-M-y 粘贴 secondary ring.
(define-key isearch-mode-map (kbd "C-M-y")  'isearch-yank-secondary)

;; 因为判断选区时, 总是首先判断 rect-mark, 因此要确保 rect-mark 的 device 后定义
(advice-add 'kill-ring-save :around
            (lambda (orig-fn &rest args)
              "Let 'kill-ring-save support rect-mark."
              (if (and (called-interactively-p 'interactive) rectangle-mark-mode)
                  (call-interactively #'copy-rectangle-as-kill)
                (apply orig-fn args)))
            '((name . rect-mark)))

(advice-add 'kill-region :around
            (lambda (orig-fn &rest args)
              "Let 'kill-region support rect-mark."
              (if (and (called-interactively-p 'interactive) rectangle-mark-mode)
                  (call-interactively #'kill-rectangle)
                (apply orig-fn args)))
            '((name . rect-mark)))

(advice-add 'delete-char :around
            (lambda (orig-fn &rest args)
              "Let 'delete-char support rect-mark."
              (if (and (called-interactively-p 'interactive) rectangle-mark-mode)
                  (delete-rectangle (region-beginning) (region-end))
                (apply orig-fn args)))
            '((name . rect-mark)))

(provide 'rect-mark_init)
;;; rect-mark_init.el ends here
