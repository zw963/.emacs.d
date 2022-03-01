;; (require 'workgroups)
;; (setq wg-prefix-key (kbd "C-c w"))
;; (setq wg-switch-on-load nil)
;; (setq wg-morph-on nil)
;; (workgroups-mode)
;; (wg-load (expand-file-name "workspace"))


(require 'workgroups2)

;; (setq wg-prefix-key "C-c z")

(setq wg-no-confirm-on-destructive-operation t)

(setq wg-session-file "~/.emacs.d/.emacs_workgroups")

(workgroups-mode 1)

(add-hook 'kill-emacs-hook #'(lambda ()
                               (wg-create-workgroup "wg0")
                               ))

(global-set-key [(f6)] 'wg-open-workgroup)

(provide 'workgroups_init)
;;; workgroups_init.el ends here
