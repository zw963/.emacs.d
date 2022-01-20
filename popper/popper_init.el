(require 'popper)
(require 'popper-echo)

;; 注意，有一些模式使用 mode-name 方式是无法生效的.
(setq popper-reference-buffers
      '("\\*Messages\\*$"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*quickrun\\*$"
        vterm-mode
        inf-ruby-mode
        (special-mode . hide)
        ;; (hover-mode . hide)
        ;; (compilation-mode . hide)
        ;; help-mode
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

;; use shackle control popup show window
(setq popper-display-control nil)
(setq popper-window-height 50)

(global-set-key [(shift control t)] 'popper-toggle-latest)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map [(shift control t)] 'popper-toggle-latest))
(global-set-key (kbd "C-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(setq popper-group-function #'popper-group-by-directory)
(popper-mode +1)
(popper-echo-mode +1)

(provide 'popper_init)

;;; popper_init.el ends here
