(require 'aggressive-indent)

(dolist (hook '(ruby-mode-hook
                enh-ruby-mode-hook
                emacs-lisp-mode-hook
                rustic-mode-hook
                rust-mode-hook
                ))
  (add-hook hook (lambda ()
                   (aggressive-indent-mode)
                   )))

(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c++-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                           (thing-at-point 'line)))))

;; (setq aggressive-indent-dont-electric-modes t)

;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'slim-mode)

;; conflict with auto-indent-mode,  and disable electric in ruby-mode, disabled.
;; (global-aggressive-indent-mode 1)

(provide 'aggressive-indent_init)
;;; aggressive-indent_init.el ends here
