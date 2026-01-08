;; -*- lexical-binding: t; -*-

;; 默认不设置 treesit-extra-load-path, 使用 ~/.emacs.d/tree-sitter 文件夹
;; (setq treesit-extra-load-path `(,(concat default-directory "tree-sitter")))


;; Emacs 29+ built-in tree-sitter
(require 'treesit)

;; 可选：显式指定 grammar 动态库放哪/从哪找（不设也行，Emacs 默认会用 ~/.emacs.d/tree-sitter）
;; (add-to-list 'treesit-extra-load-path
;;              (expand-file-name "tree-sitter" user-emacs-directory))

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
