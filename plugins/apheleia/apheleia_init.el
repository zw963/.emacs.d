(require 'apheleia)

(add-to-list 'apheleia-mode-alist '(enh-ruby-mode . prettier))

(add-to-list 'apheleia-mode-alist '(dart-mode . dart-format))
(add-to-list 'apheleia-formatters '(dart-format . ("flutter" "format" filepath)))

(apheleia-global-mode +1)

(provide 'apheleia_init)

;;; apheleia_init.el ends here
