(require 'haskell-mode-autoloads)

;; (add-to-list 'Info-default-directory-list "~/lib/emacs/haskell-mode/")

(with-eval-after-load 'flycheck
  (require 'flycheck-haskell)
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
  )

(provide 'haskell-mode_init)

;;; haskell-mode_init.el ends here
