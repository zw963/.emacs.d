(require 'flyover)

;; Use theme colors for error/warning/info faces
(setq flyover-use-theme-colors t)

;; Adjust background lightness (lower values = darker)
(setq flyover-background-lightness 45)

;; Make icon background darker than foreground
(setq flyover-percent-darker 40)

(setq flyover-text-tint 'lighter) ;; or 'darker or nil

;; "Percentage to lighten or darken the text when tinting is enabled."
(setq flyover-text-tint-percent 50)

(setq flyover-levels '(error warning))    ; Show only errors and warnings

(add-hook 'flycheck-mode-hook #'flyover-mode)

(provide 'flyover_init)

;;; flyover_init.el ends here
