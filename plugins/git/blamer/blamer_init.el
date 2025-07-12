;; -*- lexical-binding: t; -*-

(require 'blamer)

;;(setq blamer-type 'posframe-popup)
;; (setq blamer-show-avatar-p nil)
(setq blamer-idle-time 1)
(setq blamer-max-lines 1)

;; (global-set-key [(control x) (v) (m)] 'blamer-show-posframe-commit-info)
;; (setq blamer-face '((t :foreground "#7a88cf"
;;                        :background nil
;;                        :height 140
;;                        :italic t)))

(global-blamer-mode 1)

;; ;; -----------------------------------------------------------------

;; (require 'git-messenger)

;; (setq git-messenger:show-detail t)
;; (setq popup-enable-display-line-number-mode-p t)
;; (global-set-key [(control x) (v) (m)] 'git-messenger:popup-message)

(provide 'blamer_init)

;;; blamer_init.el ends here
