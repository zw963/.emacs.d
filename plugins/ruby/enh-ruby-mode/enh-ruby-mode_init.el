(require 'enh-ruby-mode)

(with-eval-after-load 'enh-ruby-mode
  (setq enh-ruby-deep-indent-paren nil)  ; 关闭括号中的列表深度缩进.
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (setq enh-ruby-program "/home/zw963/utils/ruby_tools/bin/ruby-portable")
  ;; (setq enh-ruby-hanging-brace-deep-indent-level 4)
  ;; (setq enh-ruby-bounce-deep-indent t)
  (setq enh-ruby-mode-syntax-table
        (let ((table enh-ruby-mode-syntax-table))
          ;; (modify-syntax-entry ?/ "\"" table)
          (modify-syntax-entry ?! "_"  table)
          (modify-syntax-entry ?= "_"  table)
          (modify-syntax-entry ?? "_"  table)
          (modify-syntax-entry ?$  "'"  table)
          (modify-syntax-entry ?:  "'"  table)
          (modify-syntax-entry ?@  "'"  table)
          table))
  )

(defun enh-ruby-end-of-defun-hacked (&optional arg)
  (interactive "^p")
  (call-interactively 'enh-ruby-end-of-defun)
  (call-interactively 'enh-ruby-end-of-defun)
  (call-interactively 'enh-ruby-backward-sexp)
  )

(defun enh-ruby-mode-init()
  (interactive)
  ;; (local-set-key [(control meta e)] 'enh-ruby-end-of-defun-hacked)
  ;; (local-set-key [(control c) (control p)] 'enh-ruby-beginning-of-defun)
  ;; (local-set-key [(control c) (control n)] 'enh-ruby-end-of-defun-hacked)

  ;; 这个在有些情况下, indent 会失效, 还是使用 format-buffer 较好.
  ;; (local-set-key [(control c) (control c)] 'enh-ruby-indent-exp)
  )

(with-eval-after-load 'auto-complete
  (add-to-list 'ac-modes 'enh-ruby-mode))

;; FIXME: 这是什么鬼?, 为什么之前要换绑?
;; (define-key enh-ruby-mode-map (kbd "C-c C-f") 'nil)

(provide 'enh-ruby-mode_init)
;;; enh-ruby-mode_init.el ends here
