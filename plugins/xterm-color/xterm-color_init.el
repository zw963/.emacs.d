(require 'xterm-color)

(setq compilation-environment '("TERM=xterm-256color"))

(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)
(add-hook 'quickrun-after-run-hook
          '(lambda () (advice-add 'quickrun--default-filter :around #'my/advice-compilation-filter)))

(provide 'xterm-color_init)

;;; xterm-color_init.el ends here
