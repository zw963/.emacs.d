(require 'dumb-jump)

(setq dumb-jump-force-searcher 'rg)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(provide 'dumb-jump_init)

;;; dumb-jump_init.el ends here
