;; 这个和 emacs 自带的 C-x v g 重叠. (vc-annotate.el)

;; TODO: emacs switch same buffer 坏掉了.

;; (require 'mo-git-blame)

;; (setq mo-git-blame-delete-other-windows t)
;; (global-set-key [(control x) (v) (g)] 'mo-git-blame-current)
;; (define-key mo-git-blame-mode-map [(n)] 'next-line)
;; (define-key mo-git-blame-mode-map [(p)] 'previous-line)

;; (define-key mo-git-blame-mode-map [(meta g)] 'mo-git-blame-goto-line)
;; (define-key mo-git-blame-mode-map [(d)] 'bc3-gd1-file-at-point)

(provide 'mo-git-blame_init)

;;; mo-git-blame_init.el ends here
