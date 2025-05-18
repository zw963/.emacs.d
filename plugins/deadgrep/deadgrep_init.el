;; -*- lexical-binding: t; -*-

(require 'deadgrep)

(global-set-key [(control meta r)] 'deadgrep)

(define-key deadgrep-mode-map [(f3)] 'deadgrep-edit-mode)

(provide 'deadgrep_init)

;;; deadgrep_init.el ends here
