(require 'popper)
(require 'popper-echo)

(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        hover-mode
        help-mode
        compilation-mode))

(global-set-key (kbd "C-`") 'popper-toggle-latest)
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(popper-mode +1)
(popper-echo-mode +1)


(provide 'popper_init)

;;; popper_init.el ends here
