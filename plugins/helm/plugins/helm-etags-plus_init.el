;; robe 应该完整的替代了 ctags-update 的功能。
;; 但是 robe 很慢, 而且有些非 Rails 项目，无法动态加载的情况下，TAGS 仍旧是较好的方式。
(require 'helm-etags-plus)

;; (defun helm-etags-plus-get-symbal-at-point()
;;   (let(symbol )
;;     (cond ( (equal major-mode 'verilog-mode)
;;             (with-syntax-table (copy-syntax-table (syntax-table))
;;               (modify-syntax-entry ?`  ".");treat . as punctuation character
;;               (setq symbol (thing-at-point 'symbol))))
;;           ( (member major-mode '(ruby-mode enh-ruby-mode))
;;             (modify-syntax-entry ?:  "|")
;;             (setq symbol (thing-at-point 'symbol))
;;             )
;;           (t
;;            (setq symbol (thing-at-point 'symbol))))
;;     symbol))

;; (setq helm-etags-plus-use-absolute-path t)

(defun turn-on-helm-etags-plus ()
  (local-set-key "\M-." 'helm-etags-plus-select)
  (local-set-key "\M-," 'helm-etags-plus-history-go-back)
  ;; list all visited tags
  (local-set-key "\M-*" 'helm-etags-plus-history)
  ;; go forward directly
  (global-set-key "\M-/" 'helm-etags-plus-history-go-forward)
  )

(add-hook 'ruby-mode-hook 'turn-on-helm-etags-plus)
(add-hook 'enh-ruby-mode-hook 'turn-on-helm-etags-plus)

(provide 'helm-etags-plus_init)
;;; helm-etags-plus.el ends here
