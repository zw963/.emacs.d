(require 'buffer-move)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; windmove 包是内置的
(global-set-key (kbd "M-k")     'windmove-up)
(global-set-key (kbd "M-j")   'windmove-down)
(global-set-key (kbd "M-h")   'windmove-left)
(global-set-key (kbd "M-l")  'windmove-right)


(provide 'buffer-move_init)

;;; buffer-move_init.el ends here
