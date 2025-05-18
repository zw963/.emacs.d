;; -*- lexical-binding: t; -*-

(require 'webpaste)

;; (setq webpaste-provider-priority '("paste.mozilla.org"))
(setq webpaste-provider-priority '("dpaste.org"))
;; (setq webpaste-provider-priority '("gist.github.com"))

(global-set-key (kbd "C-c p") 'webpaste-paste-buffer-or-region)

(provide 'webpaste_init)

;;; webpaste_init.el ends here
