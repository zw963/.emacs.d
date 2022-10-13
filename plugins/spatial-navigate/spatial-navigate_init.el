;; seem like block-nav.el

(require 'spatial-navigate)

(global-set-key (kbd "<M-up>") 'spatial-navigate-backward-vertical-bar)
(global-set-key (kbd "<M-right>") 'spatial-navigate-forward-horizontal-bar)
(global-set-key (kbd "<M-down>") 'spatial-navigate-forward-vertical-bar)
(global-set-key (kbd "<M-left>") 'spatial-navigate-backward-horizontal-bar)

(provide 'spatial-navigate_init)

;;; spatial-navigate_init.el ends here
