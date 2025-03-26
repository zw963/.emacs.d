(require 'blamer)

(setq blamer-show-avatar-p nil)

(global-set-key [(control x) (v) (m)] 'blamer-show-posframe-commit-info)

(global-blamer-mode 1)

(provide 'blamer_init)

;;; blamer_init.el ends here
