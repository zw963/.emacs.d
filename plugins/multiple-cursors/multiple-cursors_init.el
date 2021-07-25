(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; 用法, 先给你要选择的 symbol 设置 mark, 然后按下 C->
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "M-j") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-u C-@") 'set-rectangular-region-anchor)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(setq mc/list-file (expand-file-name "plugins/multiple-cursors/.mc-lists.el"))

(add-hook 'activate-mark-hook '(lambda ()
                                 (local-set-key (kbd "C-@") 'set-rectangular-region-anchor)
                                 ))
(add-hook 'deactivate-mark-hook '(lambda ()
                                   (local-unset-key (kbd "C-@"))
                                   ))


;; (require 'region-bindings-mode)

;; (define-key region-bindings-mode-map (kbd"C-x h") 'mc/mark-all-like-this)
;; (define-key region-bindings-mode-map (kbd "C-p") 'mc/mark-previous-like-this)
;; (define-key region-bindings-mode-map (kbd "C-n") 'mc/mark-next-like-this)
;; (define-key region-bindings-mode-map (kbd "C-m") 'mc/mark-more-like-this-extended)

;; (region-bindings-mode-enable)

(provide 'multiple-cursors_init)



;;; multiple-cursors_init.el ends here
