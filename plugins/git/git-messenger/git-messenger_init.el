(require 'git-messenger)

(setq git-messenger:show-detail t)
(setq popup-enable-display-line-number-mode-p t)
(global-set-key [(control x) (v) (m)] 'git-messenger:popup-message)

(provide 'git-messenger_init)

;;; git-messenger_init.el ends here
