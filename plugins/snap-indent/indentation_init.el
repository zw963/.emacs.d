;; -*- lexical-binding: t; -*-

(require 'snap-indent)

(add-hook 'prog-mode-hook #'snap-indent-mode)

;; 这个打开的话, 当和 iedit 一起工作时, 经常会搞乱
;; (require 'indentinator)
;; (add-hook 'prog-mode-hook #'indentinator-mode)

(provide 'indentation_init)

;;; indentation_init.el ends here
