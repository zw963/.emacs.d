;; -*- lexical-binding: t; -*-

(require 'symbol-overlay)

;; "i" ->> symbol-overlay-put
;; "n" ->> symbol-overlay-jump-next
;; "p" ->> symbol-overlay-jump-prev
;; "w" ->> symbol-overlay-save-symbol
;; "t" ->> symbol-overlay-toggle-in-scope
;; "e" ->> symbol-overlay-echo-mark
;; "d" ->> symbol-overlay-jump-to-definition
;; "s" ->> symbol-overlay-isearch-literally
;; "q" ->> symbol-overlay-query-replace
;; "r" ->> symbol-overlay-rename
;; (define-key map (kbd "<") 'symbol-overlay-jump-first)
;; (define-key map (kbd ">") 'symbol-overlay-jump-last)
;; (define-key map (kbd "h") 'symbol-overlay-map-help)

(dolist (hook '(prog-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (symbol-overlay-mode 1)
              )))

(global-set-key (kbd "<f5>") 'symbol-overlay-put)
(global-set-key (kbd "C-;") 'symbol-overlay-put)
(define-key symbol-overlay-map (kbd "C-g") 'symbol-overlay-remove-all)
(define-key symbol-overlay-map (kbd "M-n") 'symbol-overlay-switch-forward)
(define-key symbol-overlay-map (kbd "M-p") 'symbol-overlay-switch-backward)

(provide 'symbol-overlay_init)

;;; symbol-overlay_init.el ends here
