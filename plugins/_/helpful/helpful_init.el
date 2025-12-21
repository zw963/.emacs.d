;; -*- lexical-binding: t; -*-

(require 'helpful)

(global-set-key [remap describe-function] 'helpful-callable)
(global-set-key [remap Info-goto-emacs-command-node] 'helpful-function) ;; C-h F
(global-set-key [remap describe-variable] 'helpful-variable)
(global-set-key [remap describe-key] 'helpful-key)
(global-set-key [remap describe-coding-system] 'helpful-command) ;; Ctrl+h C

(global-set-key (kbd "C-c C-d") #'helpful-at-point)

(provide 'helpful_init)

;;; helpful_init.el ends here
