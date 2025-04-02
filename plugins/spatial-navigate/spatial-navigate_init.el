;; seem like block-nav.el

(require 'spatial-navigate)

(global-set-key (kbd "<M-up>") 'spatial-navigate-backward-vertical-box)
(global-set-key (kbd "<M-right>") 'spatial-navigate-forward-horizontal-box)
(global-set-key (kbd "<M-down>") 'spatial-navigate-forward-vertical-box)
(global-set-key (kbd "<M-left>") 'spatial-navigate-backward-horizontal-box)

(provide 'spatial-navigate_init)

;;; spatial-navigate_init.el ends here
