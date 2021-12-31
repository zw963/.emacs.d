(require 'zoom)

;; (custom-set-variables
;;  '(zoom-size '(0.618 . 0.618)))

(defun size-callback ()
  (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
        (t                            '(0.5 . 0.5))))

(global-set-key (kbd "C-x +") 'zoom)

(custom-set-variables
 '(zoom-size 'size-callback)
 '(zoom-ignored-major-modes '(dired-mode markdown-mode treemacs-mode helm-mode))
 '(zoom-ignored-buffer-names '(" *MINIMAP*" " *Flutter Outline*"))
 '(zoom-ignored-buffer-name-regexps '("Flutter Outline"))
 )

(with-eval-after-load 'minimap
  (defun my/fix-minimap-size ()
    (with-selected-window (get-buffer-window " *MINIMAP*")
      (setq window-size-fixed t)
      (let ((width 30))
        (window-resize (selected-window) (- width (window-total-width)) t t))))
  (add-hook 'minimap-mode-hook 'my/fix-minimap-size))

(zoom-mode)

(provide 'zoom_init)

;;; zoom_init.el ends here
