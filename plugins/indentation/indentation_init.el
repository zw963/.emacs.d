;; (require 'snap-indent)

;; (add-hook 'prog-mode-hook #'snap-indent-mode)

(require 'indentinator)
(add-hook 'prog-mode-hook #'indentinator-mode)

(provide 'indentation_init)

;;; indentation_init.el ends here
