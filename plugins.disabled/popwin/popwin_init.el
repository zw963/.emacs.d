(require 'popwin)

;; 目前，popwin 只是被 helm 使用
(with-eval-after-load 'helm
  (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
(add-hook 'helm-after-initialize-hook (lambda ()
                                          (popwin:display-buffer helm-buffer t)
                                          (popwin-mode -1)))

 ;;  Restore popwin-mode after a Helm session finishes.
 (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))
 )

;; (popwin-mode 1)
;; (add-to-list 'popwin:special-display-config '("*Warnings*" :noselect t))

(provide 'popwin_init)

;;; popwin_init.el ends here
