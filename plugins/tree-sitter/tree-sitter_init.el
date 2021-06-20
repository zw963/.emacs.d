(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)

(add-to-list 'tree-sitter-major-mode-language-alist '(enh-ruby-mode . ruby))

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(require 'tree-sitter-indent)
(add-hook 'rust-mode-hook #'tree-sitter-indent-mode)

;; (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook rust-mode-hook))
;;   (add-hook hook `(lambda ()
;;                     ;; (tree-sitter-mode 1)
;;                     (tree-sitter-hl-mode 1)
;;                     )))

(provide 'tree-sitter_init)

;;; tree-sitter_init.el ends here
