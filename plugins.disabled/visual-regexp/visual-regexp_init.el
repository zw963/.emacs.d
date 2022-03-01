(require 'visual-regexp)
(require 'visual-regexp-steroids)

(global-set-key [remap query-replace] 'vr/query-replace)
(global-set-key [remap query-replace-regexp] 'vr/query-replace)

(global-set-key [remap isearch-forward-regexp] 'vr/isearch-forward)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] 'vr/replace)

(provide 'visual-regexp_init)

;;; visual-regexp_init.el ends here
