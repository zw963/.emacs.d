(require 'flycheck)

(global-flycheck-mode)

;; (setq-default flycheck-disabled-checkers '(yaml-ruby yaml-jsyaml yaml-yamllint))
(setq-default flycheck-disabled-checkers '(yaml-yamllint))

(setq flycheck-ruby-rubocop-executable (expand-file-name "~/utils/ruby_tools/bin/rubocop-daemon-wrapper"))
(setq flycheck-eruby-erubis-executable (expand-file-name "~/utils/ruby_tools/bin/erubis"))

(require 'flycheck-color-mode-line)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

;; (require 'flycheck-inline)
;; (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)

(with-eval-after-load 'rust-mode
  (require 'flycheck-rust)
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  )

(with-eval-after-load 'haskell-mode
  (require 'flycheck-haskell)
  (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)
  )

(provide 'flycheck_init)
;;; flycheck_init.el ends here
