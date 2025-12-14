;; -*- lexical-binding: t; -*-

;; (require 'region-bindings-mode)
;; (region-bindings-mode-enable)
;; (setq region-bindings-mode-disable-predicates '((lambda () buffer-read-only)))

(setq mc/list-file (expand-file-name "plugins/multiple-cursors/.mc-lists.el"))

(require 'multiple-cursors)

;;  在命令行下, 貌似 C-2 默认等价于: C-@，这里使用同样的绑定。
;; (define-key key-translation-map [(control \2)] [(control \@)])

;; (define-key region-bindings-mode-map [(control c) (control c)] 'mc/edit-lines)
;; (define-key region-bindings-mode-map [(control \2)] 'set-rectangular-region-anchor)
(global-set-key [(control \2)] 'set-mark-command)
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

;; 开启 rectangle-mark-mode, 快捷键: C-x SPC

;; C-x r y, yank-rectangle 粘帖选区内容
;; C-x r k, kill-rectangle 剪切选区内容
;; C-x r M-w, copy-rectangle-as-kill 复制选区内容
;; C-x r d, delete-rectangle 删除选区, 文字左移
;; C-x r N, rectangle-number-lines  选区前面插入数字
;; C-x r t, string-rectangle 在指定的选区之前添加内容.

;; 下面两个库都针对 kill-ring-save, kill-region 等函数添加了 device.
;; 因为判断选区时, 总是首先判断 rect-mark, 因此要确保 rect-mark_init 在
;; browse-kill-ring 之后 require, 行为才正确.
(require 'browse-kill-ring_init)
(require 'rect)

;; 不要把下面的函数和 multli-cursor 一起使用，那个是批量修改的，而不是批量复制粘贴的。
;; 总是首先 C-x SPC 设定选区，复制后，C-x r y 粘贴。
(defadvice kill-ring-save (around rect-mark activate)
  "Let 'kill-ring-save support rect-mark."
  (if rectangle-mark-mode
      (call-interactively 'copy-rectangle-as-kill)
    ad-do-it
    ))

(defadvice kill-region (around rect-mark activate)
  "Let 'kill-ring-save support rect-mark."
  (if rectangle-mark-mode
      (call-interactively 'kill-rectangle)
    ad-do-it
    ))

(defadvice delete-char (around rect-mark activate)
  "Let 'delete-char support rect-mark."
  (if rectangle-mark-mode
      (delete-rectangle (region-beginning) (region-end))
    ad-do-it
    ))


(provide 'multiple-cursors_init)



;;; multiple-cursors_init.el ends here
