(require 'apheleia)

;; (add-to-list 'apheleia-mode-alist '(enh-ruby-mode . prettier-ruby))
;; (add-to-list 'apheleia-formatters '(crystal-format . ("crystal" "tool" "format" filepath)))
;; (add-to-list 'apheleia-mode-alist '(crystal-mode . crystal-format))

(setf (alist-get 'dart-format apheleia-formatters)
      '("flutter" "format" filepath))

(setq apheleia-mode-alist
      (delete '(js3-mode . prettier-javascript)
              (delete '(js-mode . prettier-javascript)
                      (delete '(web-mode . prettier)
                              (delete '(ruby-mode . prettier-ruby)
                                      apheleia-mode-alist)))))
(setq apheleia-log-only-errors nil)

(add-hook 'prog-mode-hook 'apheleia-global-mode)

(provide 'apheleia_init)

;;; apheleia_init.el ends here
