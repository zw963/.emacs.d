(require 'telega)

(setq telega-chat-show-avatars t)
(setq telega-use-images t)

(defun my-telega-chat-mode ()
  (set (make-local-variable 'company-backends)
       (append (list telega-emoji-company-backend
                     'telega-company-username
                     'telega-company-hashtag)))
  (company-mode 1)
  )
(add-hook 'telega-chat-mode-hook 'my-telega-chat-mode)

(provide 'telega_init)

;;; telega_init.el ends here
