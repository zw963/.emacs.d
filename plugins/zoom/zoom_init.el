(require 'zoom)

;; (custom-set-variables
;;  '(zoom-size '(0.618 . 0.618)))

(defun size-callback ()
  (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
        (t                            '(0.5 . 0.5))))

(global-set-key (kbd "C-x +") 'zoom)

(custom-set-variables
 '(zoom-size 'size-callback)
 '(zoom-ignored-major-modes '(dired-mode markdown-mode))
 )

(zoom-mode)

(provide 'zoom_init)

;;; zoom_init.el ends here
