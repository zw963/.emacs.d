(with-eval-after-load 'helm
  (require 'amx)
  (setq amx-backend 'helm)
  (global-set-key (kbd "M-x") 'amx)
  )

(provide 'amx_init)
