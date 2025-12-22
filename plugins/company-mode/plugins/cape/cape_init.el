(require 'cape)

(with-eval-after-load 'codeium
  (defun zw/setup-capf-lsp+codeium ()
    (when (bound-and-true-p lsp-mode)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
                         #'lsp-completion-at-point
                         #'codeium-completion-at-point)
                        ;; 你想要的其它 fallback（可选）
                        #'cape-file #'cape-dabbrev))))

  (add-hook 'lsp-mode-hook #'zw/setup-capf-lsp+codeium))

(provide 'cape_init)
;;; cape_init.el ends here
