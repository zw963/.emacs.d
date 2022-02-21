(require 'multiple-cursors)

(setq mc/list-file (expand-file-name "plugins/multiple-cursors/.mc-lists.el"))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; 用法, 先给你要选择的 symbol 设置 mark, 然后按下 C->
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "M-j") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-u C-@") 'set-rectangular-region-anchor)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; 这个是用鼠标左键点一下, 就新增加一个 cursor.
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; (add-hook 'activate-mark-hook (lambda ()
;;                                 (local-set-key (kbd "C-@") 'set-rectangular-region-anchor)
;;                                 (set-cursor-color "#e52b50")
;;                                 ))

;; (add-hook 'deactivate-mark-hook (lambda ()
;;                                   (local-unset-key (kbd "C-@"))
;;                                   (set-cursor-color "#00FF00")
;;                                   ))

(provide 'multiple-cursors_init)



;;; multiple-cursors_init.el ends here
