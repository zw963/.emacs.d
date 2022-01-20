(require 'minimap)

(setq minimap-window-location 'right)
(setq minimap-width-fraction 0.07)
(setq minimap-minimum-width 5)

(global-set-key [(f10)] 'minimap-mode)

(defun initialize-minimap-delay (&optional frame)
  "run minimap mode"
  (when (display-graphic-p)
    (run-with-idle-timer 0 nil 'minimap-mode)))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'find-file-hook 'initialize-minimap-delay t)
  )

(provide 'minimap_init)

;;; minimap_init.el ends here
