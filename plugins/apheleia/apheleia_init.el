(require 'apheleia)

(add-to-list 'apheleia-mode-alist '(enh-ruby-mode . prettier-ruby))
;; (add-to-list 'apheleia-formatters '(crystal-format . ("crystal" "tool" "format" filepath)))
;; (add-to-list 'apheleia-mode-alist '(crystal-mode . crystal-format))

(setf (alist-get 'dart-format apheleia-formatters)
      '("flutter" "format" filepath))

(setq apheleia-log-only-errors nil)

(apheleia-global-mode)

(provide 'apheleia_init)

;;; apheleia_init.el ends here
