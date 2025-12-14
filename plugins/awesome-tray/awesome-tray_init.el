;; 然后你可以真的把 mode-line 关掉2（每个窗口少一行）
(setq-default mode-line-format nil)

;; 画窗口分割线（横竖都有）
(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 1
      window-divider-default-places t)
(window-divider-mode 1)

(require 'awesome-tray)

;; (setq awesome-tray-active-modules '("location" "belong" "file-path" "mode-name" "battery" "date"))

(awesome-tray-mode 1)

(provide 'awesome-tray_init)
;;; awesome-tray_init.el ends here
