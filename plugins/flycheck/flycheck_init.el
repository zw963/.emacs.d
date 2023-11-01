(require 'flycheck)

(global-flycheck-mode)

(flycheck-def-executable-var ruby-rubocop-daemon-wrapper "rubocop-daemon-wrapper")

(flycheck-define-command-checker 'ruby-rubocop-daemon-wrapper
  "A Ruby syntax and style checker using the RuboCop tool.

You need at least RuboCop 0.34 for this syntax checker.

See URL `https://rubocop.org/'."
  ;; ruby-standard is defined based on this checker
  :command '("rubocop-daemon-wrapper"
             "--display-cop-names"
             "--force-exclusion"
             "--format" "emacs"
             (config-file "--config" flycheck-rubocoprc)
             (option-flag "--lint" flycheck-rubocop-lint-only)
             ;; Rubocop takes the original file name as argument when reading
             ;; from standard input
             "--stdin" source-original)
  :standard-input t
  :working-directory #'flycheck-ruby--find-project-root
  :error-patterns flycheck-ruby-rubocop-error-patterns
  :modes '(enh-ruby-mode ruby-mode)
  :next-checkers '(
                   ;; (warning . ruby-rubocop)
                   (warning . ruby-reek)
                   (warning . ruby-rubylint)))

(run-ruby-mode-hook '(setq-local flycheck-checker 'ruby-rubocop-daemon-wrapper))

;; (setq-default flycheck-disabled-checkers '(yaml-ruby yaml-jsyaml yaml-yamllint))
;; (setq-default flycheck-disabled-checkers '(yaml-yamllint))

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
