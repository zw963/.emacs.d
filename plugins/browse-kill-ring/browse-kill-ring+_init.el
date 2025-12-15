;; -*- lexical-binding: t; -*-

;; ------------------------------ secondary  ------------------------------
;; browse-kill-ring+ 的目的是让你可以同时使用 king-ring,  secondary-ring.
;; browse-kill-ring+ 会自动尝试加载 second-sel, 并使用它作为 secondary-ring

(require 'browse-kill-ring+)

(setq browse-kill-ring-no-duplicates t)
(setq add-secondary-to-ring nil)

;; 默认, Emacs 提供了 yank-pop (Alt+y), 用来逐个的弹出 king-ring 中的内容.
;; (global-set-key [remap yank-pop] 'yank-pop-secondary-ring) ;;; Alt + y 默认弹出 secondary-ring

;; yank-pop-commands hack 了 yank-pop, 使它同时支持 king-ring, secondary-ring.
;; 实现的功能:
;; 1. C-y, 然后 M-y..M-y, 滚动 kill-ring
;; 2. C-M-y, 然后 M-y..M-y,  滚动 secondary-ring, 并滚动 secondary-ring
;; 3. 直接按下 M-y, 显示 kill-ring 列表

(global-set-key [remap yank-pop] 'yank-pop-commands)

(provide 'browse-kill-ring+_init)

;;; browse-kill-ring+_init.el ends here
