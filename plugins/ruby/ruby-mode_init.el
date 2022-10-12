(require 'ruby-mode)

;; 对应于 enh-ruby-mode 的配置.
(with-eval-after-load 'ruby-mode
  (setq
   ruby-toggle-block-space-before-parameters nil
   ruby-insert-encoding-magic-comment nil ; 不要插入 encoding comment.
   ;; ruby-use-smie nil
   ;; ruby-deep-arglist nil
   ;; ruby-deep-indent-paren nil
   ;; ruby-deep-indent-paren-style t
   )
  )
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             ;; 这个在 enh-ruby-mode 里面无需设置，但是 ruby-mode 需要设定。
;;             ;; emacs 28 之后应该也不需要了
;;             (local-set-key [(control meta f)] 'ruby-forward-sexp)
;;             (local-set-key [(control meta b)] 'ruby-backward-sexp)
;;             ))

(provide 'ruby-mode_init)

;;; ruby-mode_init.el ends here
