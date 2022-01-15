(require 'popper)
(require 'popper-echo)

(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "Scratch.txt"
        vterm-mode
        inf-ruby-mode
        quickrun--mode
        (special-mode . hide)
        (hover-mode . hide)
        (compilation-mode . hide)
        help-mode
        ))

(global-set-key [(shift control t)] 'popper-toggle-latest)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map [(shift control t)] 'popper-toggle-latest))
(global-set-key (kbd "C-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(popper-mode +1)
(popper-echo-mode +1)
(setq popper-window-height 50)

(provide 'popper_init)

;;; popper_init.el ends here
