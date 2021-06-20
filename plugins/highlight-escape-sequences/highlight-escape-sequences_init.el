(require 'highlight-escape-sequences)

(add-to-list 'hes-mode-alist `(enh-ruby-mode . ,hes-ruby-escape-sequence-keywords))
(add-hook 'prog-mode-hook 'turn-on-hes-mode)
(add-hook 'emacs-lisp-mode '(turn-off-hes-mode))

(provide 'highlight-escape-sequences_init)
;;; highlight-escape-sequences_init.el ends here
