(require 'fuz)

(unless (require 'fuz-core nil t)
  (fuz-build-and-load-dymod))

(with-eval-after-load 'helm
  (require 'helm-fuz)
  (helm-fuz-mode))

;; ivy-fuz 集成有问题, 注释掉.
;; (setq ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
;; (setq ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))

;; (with-eval-after-load 'ivy
;;   (require 'ivy-fuz)
;;   (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))

(provide 'fuz_init)

;;; fuz_init.el ends here
