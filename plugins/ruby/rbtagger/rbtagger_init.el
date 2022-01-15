(require 'rbtagger)

(setq tags-add-tables nil)
(setq tags-revert-without-query t)

(advice-add 'rbtagger-mode
            :before (lambda ()
                      (unless (and rvm--current-ruby rvm--current-gemset)
                        (rvm-activate-corresponding-ruby))
                      ))

(run-ruby-mode-hook '(ritagger-mode 1))

(add-hook 'after-save-hook
          (lambda ()
            (if (eq major-mode 'enh-ruby-mode)
                (call-interactively 'rbtagger-generate-tags))))

;; (defun turn-on-helm-etags-for-ruby ()
;;   (local-set-key "\M-." 'helm-etags-select)
;;   ;; (local-set-key "\M-," 'helm-etags-plus-history-go-back)
;;   ;; ;; list all visited tags
;;   ;; (local-set-key "\M-*" 'helm-etags-plus-history)
;;   ;; ;; go forward directly
;;   ;; (global-set-key "\M-/" 'helm-etags-plus-history-go-forward)
;;   )

(run-ruby-mode-hook '(turn-on-helm-etags-for-ruby))

(provide 'rbtagger_init)

;;; rbtagger_init.el ends here
