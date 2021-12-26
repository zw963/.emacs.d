;; (require 'smartrep)
(require 'back-button)

(setq back-button-global-keystrokes '("C-x <C-@>"))

;; other useful keybinding: C-x left, C-x right, and hydra
(global-set-key (kbd "C-x C-@") 'back-button-global)

(back-button-mode 1)

;; (with-eval-after-load 'hydra
;;   (defhydra hydra-back-button
;;     (
;;      global-map "C-x ,"
;;      :pre (setq cursor-type t)
;;      :post (setq cursor-type 'bar)
;;      )
;;     "global mark ring"
;;     ("n" back-button-global-forward "global next mark")
;;     ("p" back-button-global-backward "global previous hunk")))

(provide 'back-button_init)

;;; back-button_init.el ends here
