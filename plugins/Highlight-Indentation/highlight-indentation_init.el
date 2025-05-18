;; -*- lexical-binding: t; -*-

(require 'highlight-indentation)

(set-face-background 'highlight-indentation-face "#333333")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

(dolist (hook '(coffee-mode-hook
                feature-mode-hook
                yaml-mode-hook
                yaml-ts-mode-hook
                python-mode-hook
                dart-mode-hook
                slim-mode-hook
                web-mode
                ))
  (add-hook hook 'highlight-indentation-mode))

(provide 'highlight-indentation_init)

;;; highlight-indentation_init.el ends here
