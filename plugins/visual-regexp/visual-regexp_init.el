(require 'visual-regexp-steroids)

(global-set-key [remap query-replace] 'vr/query-replace)
(global-set-key [remap query-replace-regexp] 'vr/query-replace)

;; ;; C-[
;; (define-key esc-map (kbd "C-r") 'vr/isearch-backward)
;; (define-key esc-map (kbd "C-s") 'vr/isearch-forward)

(global-set-key [(control s)] 'vr/isearch-forward)

(provide 'visual-regexp_init)

;;; visual-regexp_init.el ends here
