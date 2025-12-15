;; -*- lexical-binding: t; -*-

;; (require 'region-bindings-mode)
;; (region-bindings-mode-enable)
;; (setq region-bindings-mode-disable-predicates '((lambda () buffer-read-only)))

(setq mc/list-file (expand-file-name "plugins/multiple-cursors/.mc-lists.el"))

(require 'multiple-cursors)

;; (define-key region-bindings-mode-map [(control c) (control c)] 'mc/edit-lines)
;; (define-key region-bindings-mode-map [(control \2)] 'set-rectangular-region-anchor)

;; Ctrl + Shift + 2, 多行编辑
(global-set-key (kbd "C-@") 'set-rectangular-region-anchor)
;; 注意用法，右边的 Shift, 加左边的 C-c C-c
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(define-key mc/keymap (kbd "<return>") nil)

;; 用法, 先给你要选择的 symbol 设置 mark, 然后按下 C->
(global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-p") 'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-u C-@") 'set-rectangular-region-anchor)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(add-hook 'sgml-mode-hook
          (lambda ()
            (local-set-key [(control c)(control r)] 'mc/mark-sgml-tag-pair)
            ))

;; 这个是用鼠标左键点一下, 就新增加一个 cursor.
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; -------------------------------------------------------

(provide 'multiple-cursors_init)
;;; multiple-cursors_init.el ends here
