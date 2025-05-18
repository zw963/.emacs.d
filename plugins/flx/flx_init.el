;; -*- lexical-binding: t; -*-

(require 'flx)

(with-eval-after-load 'ido
  (require 'flx-ido)
  (flx-ido-mode t)
  (setq ido-use-faces nil)              ; 关闭 ido 默认的高亮, 使用 flx-ido 高亮
  (setq flx-ido-threshold 6000)         ; 默认值 6000, 是一个比较保守的阈值.
  )

(require 'flx-rs)
(flx-rs-load-dyn)
(advice-add 'flx-score :override #'flx-rs-score)

;; (require 'company-fuzzy)
;; (setq company-fuzzy-sorting-backend 'flx-rs
;;       company-require-match nil
;;       company-fuzzy-prefix-on-top nil
;;       company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))

(provide 'flx_init)

;;; flx_init.el ends here
