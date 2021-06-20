(require 'anzu)

(setq anzu-search-threshold 500)
(setq anzu-replace-threshold 100)
(setq anzu-minimum-input-length 3)
(setq anzu-replace-to-string-separator " => ")
;; (setq anzu-deactivate-region nil)

(global-set-key [remap query-replace] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

(define-key isearch-mode-map [remap isearch-query-replace] 'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] 'anzu-isearch-query-replace-regexp)

(global-anzu-mode t)

(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (setq anzu-replace-at-cursor-thing 'def)
            ))

(defalias 'refactor 'anzu-query-replace-at-cursor-thing)

(provide 'anzu_init)

;;; anzu_init.el ends here
