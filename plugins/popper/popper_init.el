;; -*- lexical-binding: t; -*-

(require 'popper)
(require 'popper-echo)

(with-eval-after-load 'popper
  ;; 让 popper 只做“切换/管理”，布局继续交给 shackle
  (setq popper-display-control nil)

  ;; 注意，有一些模式使用 mode-name 方式是无法生效的.
  ;;  管理不同的窗口出现的为止，popper.el 管理可以切换的窗口。
  (setq popper-reference-buffers
        '("\\*Messages\\*$"
          "Output\\*$"
          help-mode
          compilation-mode
          "\\*quickrun\\*$"
          "\\*Async Shell Command\\*"
          "\\*ansi-term\\*"
          "\\*Alerts\\*"
          vterm-mode
          inf-ruby-mode
          special-mode
          hover-mode
          ))

  (setq popper-window-height 50)

  ;; (popper-mode t)
  (popper-echo-mode t)
  )

;; ;; popper-display-control 为 nil，导致这个无效
;; (defun popper-select-popup-at-top (buffer &optional _alist)
;;   "Display popup-buffer BUFFER at the top of the screen."
;;   (let ((win (display-buffer-in-side-window
;;               buffer
;;               `((window-height . ,popper-window-height)
;;                 (side . top)
;;                 (slot . 1)))))
;;     (select-window win)))
;; (setq popper-display-function #'popper-select-popup-at-top)

;; (global-set-key [(shift control t)] 'popper-toggle-latest)q

;; (with-eval-after-load 'vterm
;;   (define-key vterm-mode-map [(shift control t)] 'popper-toggle-latest))

;; (global-set-key (kbd "C-`") 'popper-toggle-latest)
(global-set-key (kbd "C-~") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(setq popper-group-function #'popper-group-by-directory)

(provide 'popper_init)

;;; popper_init.el ends here
