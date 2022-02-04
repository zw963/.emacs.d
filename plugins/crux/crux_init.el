(require 'crux)

(global-set-key [(control k)] 'crux-smart-kill-line)
(global-set-key [(control shift return)] 'crux-smart-open-line-above)
(global-set-key [(control return)] 'crux-smart-open-line)
(global-set-key [(control c) (r)] 'crux-rename-file-and-buffer)
(global-set-key [(shift return)] 'crux-duplicate-current-line-or-region)
(global-set-key [(shift meta return)] 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key [(control c) (k)] 'crux-kill-other-buffers)
(global-set-key [(control c) (tab)] 'crux-indent-rigidly-and-copy-to-clipboard)

(global-set-key [(control backspace)] 'crux-kill-line-backwards)
(global-set-key [(control ?\d)] 'crux-kill-line-backwards)


(provide 'crux_init)

;;; crux_init.el ends here
