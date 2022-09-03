(require 'rime)

(rime-mode 1)

(setq default-input-method "rime")

(setq rime-show-candidate 'posframe)

(global-set-key (kbd "C-SPC") 'toggle-input-method)

;;(defvar sis--for-buffer nil
;;  "Saved buffer input source.")
;;(make-variable-buffer-local 'sis--for-buffer)

;;(add-hook input-method-activate-hook (lambda ()
;;                                       (setq-local sis--for-buffer t)
;;                                       ))
;;(add-hook input-method-deactivate-hook (lambda ()
;;                                       (setq-local sis--for-buffer nil)
;;                                       ))


(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "yaheiInconsolata-14"
            :internal-border-width 10))

(setq mode-line-mule-info '((:eval (rime-lighter))))

(provide 'rime_init)

;;; rime_init.el ends here
