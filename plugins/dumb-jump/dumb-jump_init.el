(require 'dumb-jump)

(setq dumb-jump-force-searcher 'rg)

(defun set-dumb-jump-as-default ()
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 90 t)
  (setq-local xref-show-definitions-function #'xref-show-definitions-completing-read)
  )

;; (add-hook 'crystal-mode-hook 'set-dumb-jump-as-default)
(add-hook 'prog-mode-hook 'set-dumb-jump-as-default)

(provide 'dumb-jump_init)

;;; dumb-jump_init.el ends here
