(require 'quickrun)

(add-to-list 'quickrun--major-mode-alist '(enh-ruby-mode . "ruby"))

(setq quickrun-focus-p nil)

(run-ruby-mode-hook '(local-set-key [(control x) (control e)] 'quickrun-region))

(provide 'quickrun_init)

;;; quickrun_init.el ends here
