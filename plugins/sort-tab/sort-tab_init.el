;; -*- lexical-binding: t; -*-

(require 'sort-tab)

(sort-tab-mode t)

;; 这些数字是 visible 数字，不是所有 tab 的实际索引。
(global-set-key [(meta \1)] 'sort-tab-select-visible-tab)
(global-set-key [(meta \2)] 'sort-tab-select-visible-tab)
(global-set-key [(meta \3)] 'sort-tab-select-visible-tab)
(global-set-key [(meta \4)] 'sort-tab-select-visible-tab)
(global-set-key [(meta \5)] 'sort-tab-select-visible-tab)
(global-set-key [(meta \6)] 'sort-tab-select-visible-tab)
(global-set-key [(meta \7)] 'sort-tab-select-visible-tab)

(global-set-key [(meta \9)] 'sort-tab-select-prev-tab)
(global-set-key [(meta \0)] 'sort-tab-select-next-tab)

(provide 'sort-tab_init)

;;; sort-tab_init.el ends here
