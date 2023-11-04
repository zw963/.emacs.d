;; (require 'tree-sitter)

;; 默认不设置 treesit-extra-load-path, 使用 ~/.emacs.d/tree-sitter 文件夹
;; (setq treesit-extra-load-path `(,(concat default-directory "tree-sitter")))

(setq major-mode-remap-alist
      '((c-mode          . c-ts-mode)
        (tsx-mode        . tsx-ts-mode)
        (css-mode        . css-ts-mode)
        (c++-mode        . c++-ts-mode)
        (java-mode       . java-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (csharp-mode     . csharp-ts-mode)
        (python-mode     . python-ts-mode)
        ))

(provide 'tree-sitter_init)

;;; tree-sitter_init.el ends here
