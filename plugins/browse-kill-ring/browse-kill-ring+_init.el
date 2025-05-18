;; -*- lexical-binding: t; -*-

;; ------------------------------ secondary  ------------------------------
;; browse-kill-ring+ 的目的是让你可以同时使用 king-ring,  secondary-ring.
;; browse-kill-ring+ 会自动尝试加载 second-sel, 并使用它作为 secondary-ring

(require 'browser-kill-ring+)

(setq browse-kill-ring-no-duplicates t)
(setq add-secondary-to-ring nil)

(defun convert-to-secondary-region ()
  (interactive)
  (primary-to-secondary (region-beginning) (region-end))
  (delete-overlay mouse-secondary-overlay)
  (deactivate-mark))

;; 这个功能会造成 meta + w 复制内容之后, 不会自动的 deactivate mark
;; 因此 deactivate-mark-hook 中的代码不会运行.
(defadvice kill-ring-save (around secondary-ring activate)
  "Let 'kill-ring-save support secondary ring."
  (when (use-region-p) (convert-to-secondary-region))
  ad-do-it
  )
(defadvice kill-region (around secondary-ring activate)
  "Let 'kill-region support secondary ring."
  (when (use-region-p) (convert-to-secondary-region))
  ad-do-it
  )

(global-set-key [(control meta y)] 'yank-secondary) ; C-M-y 粘贴 secondary ring.
(define-key isearch-mode-map (kbd "C-M-y")  'isearch-yank-secondary)

;; 默认, Emacs 提供了 yank-pop (Alt+y), 用来逐个的弹出 king-ring 中的内容.
;; (global-set-key [remap yank-pop] 'yank-pop-secondary-ring) ;;; Alt + y 默认弹出 secondary-ring

;; yank-pop-commands hack 了 yank-pop, 使它同时支持 king-ring, secondary-ring.
;; 实现的功能:
;; 1. C-y, 然后 M-y..M-y, 滚动 kill-ring
;; 2. C-M-y, 然后 M-y..M-y,  滚动 secondary-ring, 并滚动 secondary-ring
;; 3. 直接按下 M-y, 显示 kill-ring 列表

(global-set-key [remap yank-pop] 'yank-pop-commands)

(provide 'browser-kill-ring+_init)

;;; browser-kill-ring+_init.el ends here
