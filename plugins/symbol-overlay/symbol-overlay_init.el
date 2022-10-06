(require 'symbol-overlay)

;; (global-set-key (kbd "M-i") 'symbol-overlay-put)

;; 不需要这个, 因为 n, p 就是类似功能, 下面是所有可用快捷键.

;; "i" -> symbol-overlay-put
;; "n" -> symbol-overlay-jump-next
;; "p" -> symbol-overlay-jump-prev
;; "w" -> symbol-overlay-save-symbol
;; "t" -> symbol-overlay-toggle-in-scope
;; "e" -> symbol-overlay-echo-mark
;; "d" -> symbol-overlay-jump-to-definition
;; "s" -> symbol-overlay-isearch-literally
;; "q" -> symbol-overlay-query-replace
;; "r" -> symbol-overlay-rename
;; (define-key map (kbd "<") 'symbol-overlay-jump-first)
;; (define-key map (kbd ">") 'symbol-overlay-jump-last)
;; (define-key map (kbd "h") 'symbol-overlay-map-help)

;; (define-key symbol-overlay-map (kbd "s-n") 'symbol-overlay-jump-next)
(define-key symbol-overlay-map (kbd "C-g") 'symbol-overlay-remove-all)

(dolist (hook '(prog-mode-hook
                web-mode-hook
                ))
  (add-hook hook (lambda ()
                   (symbol-overlay-mode 1)
                   )))

(global-set-key (kbd "<f7>") 'symbol-overlay-put)

(provide 'symbol-overlay_init)

;;; symbol-overlay_init.el ends here
