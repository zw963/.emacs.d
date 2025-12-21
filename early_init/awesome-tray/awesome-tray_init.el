;; 然后你可以真的把 mode-line 关掉2（每个窗口少一行）
(setq-default mode-line-format nil)

;; 画窗口分割线（横竖都有）
(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 1
      window-divider-default-places t)
(window-divider-mode 1)

(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'initial-frame-alist '(undecorated . t)) ; 非 daemon 也顺便覆盖

(require 'awesome-tray)

;; (setq awesome-tray-position 'left)
(setq awesome-tray-file-path-show-filename t)
;; 设定一个很大值，显示完整目录名
(setq awesome-tray-file-path-truncated-name-length 30)
(setq awesome-tray-active-modules '("buffer-read-only" "iedit" "location" "buffer-name" "file-path" "flycheck" "git" "belong" "mode-name" "date"))

(awesome-tray-mode 1)

(provide 'awesome-tray_init)
;;; awesome-tray_init.el ends here


;; =============== 不用的代码 ===============

;; 这段代码和上面的 add-to-list 功能等价

;; (defun my/hide-titlebar (frame)
;;   "Make FRAME undecorated (no titlebar)."
;;   (when (display-graphic-p frame)
;;     ;; 注意：一定要改传进来的 frame
;;     (modify-frame-parameters frame '((undecorated . t)))
;;     ))

;; ;; 隐藏标题栏
;; (when (and (fboundp 'daemonp) (daemonp))
;;     (add-hook 'after-make-frame-functions 'my/hide-titlebar t)
;;   )
