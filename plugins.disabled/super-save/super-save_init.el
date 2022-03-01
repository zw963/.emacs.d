(require 'super-save)
;; (setq super-save-auto-save-when-idle t)
(add-to-list 'super-save-triggers 'ace-window)
;; (add-to-list 'super-save-hook-triggers 'find-file-hook)
(setq super-save-remote-files nil)
(setq super-save-exclude '(".gpg"))
(super-save-mode 1)

(provide 'super-save_init)
;;; super-save_init.el ends here
