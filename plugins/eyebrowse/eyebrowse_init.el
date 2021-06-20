(require 'eyebrowse)
(require 'desktop)

(setq eyebrowse-mode-line-style 'always)

;; 确保也保存 side window.
(add-to-list 'window-persistent-parameters '(window-side . writable))
(add-to-list 'window-persistent-parameters '(window-slot . writable))

(eyebrowse-mode t)

(provide 'eyebrowse_init)

;;; eyebrowse_init.el ends here
