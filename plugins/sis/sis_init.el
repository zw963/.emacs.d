(require 'sis)

(sis-ism-lazyman-config "1" "2" 'fcitx5)

(sis-global-respect-mode t)
(sis-global-cursor-color-mode t)
(sis-global-inline-mode t)

;; (add-hook 'input-method-activate-hook 'blink-cursor-mode)
;; (add-hook 'input-method-deactivate-hook (lambda ()
;;                                           (blink-cursor-mode -1)
;;                                           ))

(provide 'sis_init)

;;; sis_init.el ends here
