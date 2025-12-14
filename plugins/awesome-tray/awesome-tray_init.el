;; 然后你可以真的把 mode-line 关掉2（每个窗口少一行）
(setq-default mode-line-format nil)

;; 画窗口分割线（横竖都有）
(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 1
      window-divider-default-places t)
(window-divider-mode 1)

;; 隐藏标题栏
(when (display-graphic-p)
  (modify-frame-parameters nil '((undecorated . t))))

(require 'awesome-tray)

(setq awesome-tray-position 'left)
(setq awesome-tray-file-path-show-filename t)
;; 设定一个很大值，显示完整目录名
(setq awesome-tray-file-path-truncated-name-length 30)

;; (setq awesome-tray-active-modules '("location" "belong" "file-path" "mode-name" "battery" "date"))

;; (add-to-list 'awesome-tray-active-modules "anzu")

(awesome-tray-mode 1)

(provide 'awesome-tray_init)
;;; awesome-tray_init.el ends here
