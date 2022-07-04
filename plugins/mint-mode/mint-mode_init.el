(require 'mint-mode)

(add-hook 'mint-mode-hook (lambda ()
                            ;; 这个我改源码暂时关闭掉了
                            ;; (add-hook 'after-save-hook #'mint-format-file nil 'local)
                            (setq-local js-indent-level 2)
                            ))

(provide 'mint-mode_init)

;;; mint-mode_init.el ends here
