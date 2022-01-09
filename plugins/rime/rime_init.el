(require 'rime)

(rime-mode 1)

(setq default-input-method "rime")

(setq rime-show-candidate 'posframe)

(local-set-key (kbd "C-SPC") 'toggle-input-method)

(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "yaheiInconsolata-14"
            :internal-border-width 10))

(setq mode-line-mule-info '((:eval (rime-lighter))))

(provide 'rime_init)

;;; rime_init.el ends here
