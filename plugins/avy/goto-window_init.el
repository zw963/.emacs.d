;; ace-window 激活后，有一个有用的快捷键 m, 用来交换当前 window 和指定的 window.

(require 'ace-window)
;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-scope 'visible)
(setq aw-dispatch-always t)

;; 或者直接 C-u C-x o, 直接就是 swap window, C-u C-u C-x 0, 删除指定的 window.
(global-set-key [remap other-window] (lambda ()
                                       (interactive)
                                       (progn
                                         (setq unread-command-events (listify-key-sequence (kbd "?"))) ;; Queue "?" key command to be sent
                                         (ace-window nil))
                                       ))

;; (set-face-attribute
;;  'aw-leading-char-face nil
;;  :foreground "deep sky blue"
;;  :weight 'bold
;;  :height 3.0)

;; (set-face-attribute
;;  'aw-mode-line-face nil
;;  :inherit 'mode-line-buffer-id
;;  :foreground "lawn green")


;; windmove 包是内置的
(global-set-key (kbd "M-k")     'windmove-up)
(global-set-key (kbd "M-j")   'windmove-down)
(global-set-key (kbd "M-h")   'windmove-left)
(global-set-key (kbd "M-l")  'windmove-right)

(provide 'goto-window_init)
