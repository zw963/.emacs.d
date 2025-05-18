;; -*- lexical-binding: t; -*-

(require 'goto-chg)

(global-set-key [(control ?,)] 'goto-last-change)
(global-set-key [(control ?.)] 'goto-last-change-reverse)

(define-key org-mode-map [(control ?,)] 'goto-last-change)
(define-key org-mode-map [(control ?.)] 'goto-last-change-reverse)

(provide 'goto-chg_init)

;;; goto-chg_init.el ends here
