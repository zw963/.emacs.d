;; -*- lexical-binding: t; -*-

(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")

(require 'vterm)

;; 按下 C-c C-t, 切换到 copy 模式，此时，直接按回车的话（没有添加选区），会拷贝整行。
;; 下面的参数，会跳过提示符。
(setq
 vterm-copy-exclude-prompt t
 vterm-max-scrollback 50000
 vterm-enable-manipulate-selection-data-by-osc52 t
 )
(defun switch-vterm-and-back ()
  (interactive)
  (if (string= (buffer-name) "*vterm*")
      (previous-buffer)
    (vterm)))

;; (setq vterm-clear-scrollback-when-clearing t)
;; (setq vterm-use-vterm-prompt-detection-method nil)
;; (setq term-prompt-regexp "╰─ $ ")

;; (add-hook 'shell-mode-hook
;;           #'(lambda ()
;;               (dirtrack-mode 1)
;;               (add-hook 'comint-preoutput-filter-functions
;;                         'dirtrack-filter-out-pwd-prompt t t)))

;; (defun drop-down-term ()
;;   "Open a drop-down terminal in the same directory as the current file."
;;   (interactive)
;;   (require 'multi-vterm)
;;   (let ((win (get-local-window-for-buffer-name "*vterminal")))
;;     (if win
;;         (delete-window win)
;;       (let ((buffer (get-local-buffer-for-buffer-name "*vterminal")))
;;         (unless buffer
;;           (multi-vterm)
;;           (setq buffer (get-local-buffer-for-buffer-name "*vterminal"))
;;           )
;;         (setq win
;;               (display-buffer-in-side-window
;;                buffer
;;                '((side . top)
;;                  ;; (dedicated . t)
;;                  )))))
;;     (select-window win)))

(add-hook 'vterm-mode-hook
          (lambda ()
            (set (make-local-variable 'buffer-face-mode-face) '(:family "yaheiInconsolata"))
            (buffer-face-mode t)
            ;; 换绑快捷键的例子。
            ;; (define-key vterm-mode-map (kbd "<C-backspace>")
            ;;             (lambda () (interactive) (vterm-send-key (kbd "C-w"))))
            ))

;; 测试默认 xterm-256color 有没有性能问题。
;; (require 'eterm-256color)
;; (setq vterm-term-environment-variable "eterm-color")

(require 'multi-vterm_init)

(provide 'vterm_init)

;;; vterm_init.el ends here
