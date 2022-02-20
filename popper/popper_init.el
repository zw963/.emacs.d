(require 'popper)
(require 'popper-echo)

;; 注意，有一些模式使用 mode-name 方式是无法生效的.
;; 被这里管理的 mode, 应该都是在 shackle 中配置的 mode, 在需要的时候，希望随时可以通过快捷键调出来。
;; 它和 shackle 并不冲突，shackle 管理所有，popper.el 只管理随时想看的。
(setq popper-reference-buffers
      '("\\*Messages\\*$"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*quickrun\\*$"
        "\\*ansi-term\\*"
        "\\*Alerts\\*"
        vterm-mode
        inf-ruby-mode
        special-mode
        hover-mode
        compilation-mode
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

;; (global-set-key [(shift control t)] 'popper-toggle-latest)

;; (with-eval-after-load 'vterm
;;   (define-key vterm-mode-map [(shift control t)] 'popper-toggle-latest))

(global-set-key (kbd "C-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(setq popper-group-function #'popper-group-by-directory)
(popper-mode +1)
(popper-echo-mode +1)

(provide 'popper_init)

;;; popper_init.el ends here
