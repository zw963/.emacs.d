;; -*- lexical-binding: t; -*-

(require 'rbtagger)

(setq tags-add-tables nil)
(setq tags-revert-without-query t)

;; (advice-add 'rbtagger-mode :before
;;             (lambda ()
;;               (unless (and rvm--current-ruby rvm--current-gemset)
;;                 (rvm-activate-corresponding-ruby))
;;               ))

(defun ruby--rbtagger-after-save ()
  (when (derived-mode-p 'ruby-mode 'enh-ruby-mode 'ruby-ts-mode)
    (rbtagger-generate-tags)))

(defun ruby--enable-rbtagger-on-save ()
  ;; 修改本地 after-save-hook
  (add-hook 'after-save-hook #'ruby--rbtagger-after-save nil t))

(run-ruby-mode-hook '(ruby--enable-rbtagger-on-save))

;; (defun turn-on-helm-etags-for-ruby ()
;;   (local-set-key "\M-." 'helm-etags-select)
;;   ;; (local-set-key "\M-," 'helm-etags-plus-history-go-back)
;;   ;; ;; list all visited tags
;;   ;; (local-set-key "\M-*" 'helm-etags-plus-history)
;;   ;; ;; go forward directly
;;   ;; (global-set-key "\M-/" 'helm-etags-plus-history-go-forward)
;;   )

(provide 'rbtagger_init)

;;; rbtagger_init.el ends here
