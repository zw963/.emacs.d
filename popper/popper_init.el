(require 'popper)
(require 'popper-echo)

;; 注意，有一些模式使用 mode-name 方式是无法生效的.
(setq popper-reference-buffers
      '("\\*Messages\\*$"
        "Output\\*$"
        ;; "LSP Error List\\*$"
        "\\*Async Shell Command\\*"
        "\\*quickrun\\*$"
        "Scratch.txt"
        vterm-mode
        inf-ruby-mode
        (special-mode . hide)
        (hover-mode . hide)
        (compilation-mode . hide)
        help-mode
        ))

(defun popper-select-popup-at-top (buffer &optional _alist)
  "Display popup-buffer BUFFER at the bottom of the screen."
  (let ((win (display-buffer-in-side-window
              buffer
              `((window-height . ,popper-window-height)
                (side . top)
                (slot . 1)))))
    (select-window win)))
(setq popper-display-function #'popper-select-popup-at-top)

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
