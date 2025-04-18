(require 'blamer)

(setq blamer-show-avatar-p nil)
(setq blamer-idle-time 1)
(setq blamer-max-lines 1)

(global-set-key [(control x) (v) (m)] 'blamer-show-posframe-commit-info)

(setq blamer-face '((t :foreground "#7a88cf"
                       :background nil
                       :height 140
                       :italic t)))

(global-blamer-mode 1)

(provide 'blamer_init)

;;; blamer_init.el ends here
