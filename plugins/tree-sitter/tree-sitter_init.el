;; -*- lexical-binding: t; -*-

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
        (sh-mode         . bash-ts-mode)
        ))

;; (dolist (pair '((c-mode . c-ts-mode)
;;                 (c++-mode . c++-ts-mode)
;;                 (css-mode . css-ts-mode)
;;                 (python-mode . python-ts-mode)
;;                 (sh-mode . bash-ts-mode)
;;                 (java-mode . java-ts-mode)
;;                 (cmake-mode . cmake-ts-mode)
;;                 (csharp-mode . csharp-ts-mode)
;;                 (tsx-mode . tsx-ts-mode)))
;;   (add-to-list 'major-mode-remap-alist pair))


(provide 'tree-sitter_init)

;;; tree-sitter_init.el ends here
