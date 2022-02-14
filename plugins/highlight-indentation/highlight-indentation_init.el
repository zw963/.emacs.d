(require 'highlight-indentation)

;; (set-face-background 'highlight-indentation-face "#333333")
;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

(dolist (hook '(coffee-mode-hook
                feature-mode-hook
                yaml-mode-hook
                python-mode-hook
                slim-mode-hook))
  (add-hook hook 'highlight-indentation-mode))

(provide 'highlight-indentation_init)

;;; highlight-indentation_init.el ends here
