;; -*- lexical-binding: t; -*-

(require 'iedit)

(setq-default iedit-occurrence-context-lines 0)
(setq iedit-auto-narrow t)
;; (setq iedit-auto-buffering t) ;; 不知道干嘛的，打开 iedit 编辑无效了

(require 'iedit-rect)
;; 开启 iedit-rectangle-mode 之后, 可以直接编辑, 编辑完记住要再次快捷键退出模式.
(global-set-key [(control meta \7)] 'iedit-rectangle-mode)
(global-set-key (kbd "C-;") 'iedit-mode) ;; 这一行不是必须的，不过为了覆盖其他快捷键，加上。
(define-key iedit-lib-keymap [(meta p)] 'iedit-prev-occurrence)
(define-key iedit-lib-keymap [(meta n)] 'iedit-next-occurrence)

(provide 'iedit_init)
;;; iedit_init.el ends here
