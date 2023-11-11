(require 'symbol-overlay)

(global-set-key (kbd "C-SPC") 'symbol-overlay-put)
(global-set-key (kbd "<f7>") 'symbol-overlay-remove-all)
(define-key symbol-overlay-map (kbd "M-n") 'symbol-overlay-switch-forward)
(define-key symbol-overlay-map (kbd "M-p") 'symbol-overlay-switch-backward)

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

(dolist (hook '(prog-mode-hook
                web-mode-hook
                ))
  (add-hook hook (lambda ()
                   (symbol-overlay-mode 1)
                   )))

(global-set-key (kbd "<f7>") 'symbol-overlay-put)
;; (global-set-key (kbd "C-;") 'symbol-overlay-put)
(define-key symbol-overlay-map (kbd "C-g") 'symbol-overlay-remove-all)

(provide 'symbol-overlay_init)

;;; symbol-overlay_init.el ends here
