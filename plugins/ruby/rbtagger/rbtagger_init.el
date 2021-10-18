(require 'rbtagger)

(setq tags-add-tables nil)
(setq tags-revert-without-query t)
(add-hook 'ruby-mode-hook 'rbtagger-mode)
(add-hook 'enh-ruby-mode-hook 'rbtagger-mode)

(add-hook 'after-save-hook
          (lambda ()
            (if (eq major-mode 'enh-ruby-mode)
                (call-interactively 'rbtagger-generate-tags))))

(provide 'rbtagger_init)

;;; rbtagger_init.el ends here
