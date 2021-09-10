(require 'back-button)

(back-button-mode 1)

(with-eval-after-load 'hydra
  (defhydra hydra-back-button
    (
     global-map "C-x ,"
     :pre (setq cursor-type t)
     :post (setq cursor-type 'bar)
     )
    "global mark ring"
    ("n" back-button-global-forward "global next mark")
    ("p" back-button-global-backward "global previous hunk")))

(provide 'back-button_init)

;;; back-button_init.el ends here
