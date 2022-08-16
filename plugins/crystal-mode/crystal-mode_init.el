(require 'crystal-mode)
;; (require 'lsp-crystal_init)
(require 'flycheck-crystal)
(require 'crystal-mode_keyword_highlight_init)

;; (require 'ameba)
;; (require 'flycheck-ameba)
;; (flycheck-ameba-setup)

(add-hook 'crystal-mode-hook
          (lambda ()
            ;; 如果开启了 lsp 里面的 lsp-format-buffer, 则关闭这个。
            (add-hook 'before-save-hook #'crystal-tool-format nil 'local)
            ))

(provide 'crystal-mode_init)

;;; crystal-mode_init.el ends here
