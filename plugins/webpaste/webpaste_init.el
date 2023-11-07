(require 'webpaste)

(setq webpaste-provider-priority '("paste.mozilla.org" "dpaste.org"))

(global-set-key (kbd "C-c p") 'webpaste-paste-buffer-or-region)

(provide 'webpaste_init)

;;; webpaste_init.el ends here
