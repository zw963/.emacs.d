;; -*- lexical-binding: t; -*-

(require 'golden-ratio)

(golden-ratio-mode 1)


;; zoom 和 helm 工作的不好，
;; (require 'zoom)

;; (defun size-callback ()
;;   (cond ((> (frame-pixel-width) 1280) '(90 . 0.618))
;;         (t                            '(0.5 . 0.5))))

;; (custom-set-variables
;;  '(zoom-size 'size-callback)
;;  '(zoom-ignored-buffer-name-regexps '("^*helm" "^helm"))
;;  )

;; (global-set-key (kbd "C-x +") 'zoom)

;; (zoom-mode t)

(provide 'golden-ratio_init)

;;; golden-ratio_init.el ends here
