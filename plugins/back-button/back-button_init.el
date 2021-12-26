(require 'back-button)

(setq back-button-global-keystrokes '("C-x <C-@>"))

;; other useful keybinding: C-x left, C-x right, and hydra
(global-set-key (kbd "C-x C-@") 'back-button-global)

(back-button-mode 1)

(provide 'back-button_init)

;;; back-button_init.el ends here
