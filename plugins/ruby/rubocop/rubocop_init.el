;; -*- lexical-binding: t; -*-

(require 'rubocop)

;; (setq rubocop-format-on-save t)

(setq rubocop-check-command "rubocop-daemon-wrapper --format emacs")
(setq rubocop-autocorrect-command "rubocop-daemon-wrapper -a --format emacs")
(setq rubocop-format-command "rubocop-daemon-wrapper -x --format emacs")

(run-ruby-mode-hook '(rubocop-mode))

;; (run-ruby-mode-hook '(local-set-key [(control c) (control c)] 'rubocop-format-current-file))

(provide 'rubocop_init)

;;; rubocop_init.el ends here
