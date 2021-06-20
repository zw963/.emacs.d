(require 'dpaste)

;; It use user-full-name variable.
(global-set-key (kbd "C-c p") 'dpaste-region-or-buffer)

(add-to-list 'dpaste-supported-modes-alist '(enh-ruby-mode . "rb"))

(provide 'dpaste_init)
;;; dpaste_init.el ends here
