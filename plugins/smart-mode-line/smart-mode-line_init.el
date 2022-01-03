;; ------------------------------ 模式行美化 ------------------------------
;; 这个包造成 delete-selection-mode 关闭，需要更新.

(require 'smart-mode-line)
(setq sml/theme 'smart-mode-line-powerline)
;; (setq sml/theme 'smart-mode-line-atom-one-dark)

;; 这个可能需要根据屏幕来做调整, 确保没有 mode line 选项跑到屏幕右侧外面.
(setq sml/mode-width 25)
;; (setq sml/vc-mode-show-backend t)

(setq
 sml/no-confirm-load-theme t
 sml/name-width 55
 ;; sml/shorten-directory t
 ;; sml/shorten-modes t

 ;; mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client
 ;;                    mode-line-modified mode-line-remote mode-line-frame-identification
 ;;                    mode-line-buffer-identification "   " (10 "(%P)")
 ;;                    (vc-mode vc-mode)
 ;;                    "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
 )

(add-to-list 'sml/replacer-regexp-list '("^~/Airhost/airhost_ror" ":AH:"))

;; (require 'nyan-mode)
;; (add-to-list 'mode-line-format (list '(:eval (list (nyan-create)))))

(sml/setup)

(provide 'smart-mode-line_init)
;;; smart-mode-line_init.el ends here
