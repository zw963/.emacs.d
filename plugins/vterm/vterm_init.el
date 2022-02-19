(require 'vterm)

;; How to build:

;; cd emacs-libvterm
;; mkdir -p build
;; cd build
;; cmake -DUSE_SYSTEM_LIBVTERM=no ..
;; make

;; 按下 C-c C-t, 切换到 copy 模式，此时，直接按回车的话（没有添加选区），会拷贝整行。
;; 下面的参数，会跳过提示符。
(setq vterm-copy-exclude-prompt t)
(setq vterm-max-scrollback 50000)
(defun switch-vterm-and-back ()
  (interactive)
  (if (string= (buffer-name) "*vterm*")
      (previous-buffer)
    (vterm)))

(setq vterm-clear-scrollback-when-clearing t)

;; (setq vterm-use-vterm-prompt-detection-method nil)
;; (setq term-prompt-regexp "╰─ $ ")

;; (setq vterm-enable-manipulate-selection-data-by-osc52 t)

;; (add-hook 'shell-mode-hook
;;           #'(lambda ()
;;               (dirtrack-mode 1)
;;               (add-hook 'comint-preoutput-filter-functions
;;                         'dirtrack-filter-out-pwd-prompt t t)))

(add-hook 'vterm-mode-hook (lambda ()
                             (define-key vterm-mode-map [(control shift k)] 'vterm-clear)
                             ;; (dirtrack-mode 1)
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

;; 这个不用了，使用 popper.el 管理。

(require 'multi-vterm_init)

(provide 'vterm_init)

;;; vterm_init.el ends here
