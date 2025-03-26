(setq back-button-local-keystrokes nil)

(require 'back-button)

;; (setq back-button-global-keystrokes '("C-x <C-@>"))

;; 这个和 smartrep.el 一起使用，C-x left, C-x right(无需一直按下 C-x) 以及 C-x C-left, C-x C-right.
;;
;; (global-set-key (kbd "C-x C-@") 'back-button-global)

(back-button-mode 1)

(provide 'back-button_init)

;;; back-button_init.el ends here
