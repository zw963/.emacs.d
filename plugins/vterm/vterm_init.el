(require 'vterm)

;; 按下 C-c C-t, 切换到 copy 模式，此时，直接按回车的话（没有添加选区），会拷贝整行。
;; 下面的参数，会跳过提示符。
(setq vterm-copy-exclude-prompt t)
(setq vterm-max-scrollback 10000)
;; (setq vterm-kill-buffer-on-exit  nil)

(defun switch-vterm-and-back ()
  (interactive)
  (if (string= (buffer-name) "*vterm*")
      (previous-buffer)
    (vterm)))

;; 使用 Ctrl+x j, Ctrl+x l 来切换 buffer 和 vterm.
(global-set-key [(control return)] 'switch-vterm-and-back)


(setq vterm-clear-scrollback t)

(add-hook 'vterm-mode-hook '(lambda ()
                              (define-key vterm-mode-map [(control shift k)] 'vterm-clear)
                              ))

(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")

;; Add this piece of code to your configuration file to make counsel use the
;; correct function to yank in vterm buffers.
(defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  (if (equal major-mode 'vterm-mode)
      (let ((inhibit-read-only t)
            (yank-undo-function (lambda (_start _end) (vterm-undo))))
        (cl-letf (((symbol-function 'insert-for-yank)
                   (lambda (str) (vterm-send-string str t))))
          (apply orig-fun args)))
    (apply orig-fun args)))

(advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

(provide 'vterm_init)

;;; vterm_init.el ends here
