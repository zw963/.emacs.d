;; -*- lexical-binding: t; -*-

(require 'iedit)

;; 如果退出 iedit 之后，执行 undo 时，一次性 undo 所有 iedit 里面的修改。
;; 老的行为是，所有 iedit 的操作都作为历史， 退出后 undo，仍旧是一点一点 undo

(setq my-buffer-undo-list nil)
(advice-add 'iedit-mode :before '(lambda (&rest args) ;; save current
                                   (setq my-buffer-undo-list buffer-undo-list)))

(advice-add 'iedit-mode :after '(lambda (&rest args)  ;; restore previously saved
                                  (setq buffer-undo-list my-buffer-undo-list)))

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
