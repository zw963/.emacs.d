(require 'quickrun)

(add-to-list 'quickrun--major-mode-alist '(enh-ruby-mode . "ruby"))

;; (setq quickrun-focus-p nil)

(dolist (hook '(
                ruby-mode-hook
                enh-ruby-mode-hook
                dart-mode-hook
                go-mode-hook
                ))
  (add-hook hook (lambda ()
                   (local-set-key [(control x) (control e)] 'quickrun-region)
                   )))

(provide 'quickrun_init)

;;; quickrun_init.el ends here
