(require 'quickrun)

(setq quickrun-timeout-seconds nil)

(add-to-list 'quickrun--major-mode-alist '(enh-ruby-mode . "ruby"))

;; (setq quickrun-focus-p nil)

(setq quickrun-debug t)

(dolist (hook '(
                ruby-mode-hook
                enh-ruby-mode-hook
                dart-mode-hook
                go-mode-hook
                sh-mode-hook
                crystal-mode-hook
                ))
  (add-hook hook (lambda ()
                   (local-set-key [(control x) (control e)] 'quickrun-region)
                   (local-set-key [(control c) (return)] 'quickrun)
                   (local-set-key [(control c) (tab)] 'quickrun-compile-only)
                   )))


(add-hook 'quickrun-after-run-hook 'say)

(provide 'quickrun_init)

;;; quickrun_init.el ends here
