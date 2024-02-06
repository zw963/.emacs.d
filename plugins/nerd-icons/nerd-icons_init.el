(require 'nerd-icons)

(require 'nerd-icons-completion)
(nerd-icons-completion-mode)

(require 'nerd-icons-ibuffer)
(add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)

(require 'nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

(with-eval-after-load 'treemacs
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons")
  )

(provide 'nerd-icons_init)

;;; nerd-icons_init.el ends here
