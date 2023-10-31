(require 'flx)

(with-eval-after-load 'ido
  (require 'flx-ido)
  (flx-ido-mode t)
  )

(require 'flx-rs)
(flx-rs-load-dyn)
(advice-add 'flx-score :override #'flx-rs-score)

(require 'company-fuzzy)
(setq company-fuzzy-sorting-backend 'flx-rs
      company-fuzzy-prefix-on-top nil
      company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))

(provide 'flx_init)

;;; flx_init.el ends here
