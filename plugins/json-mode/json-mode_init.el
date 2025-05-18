;; -*- lexical-binding: t; -*-

(require 'json-mode)

(defun turn-on-format-json ()
  (local-set-key [(control c) (control c)] 'json-mode-beautify))

(add-hook 'json-mode-hook 'turn-on-format-json)

(provide 'json-mode_init)

;;; json-mode_init.el ends here
