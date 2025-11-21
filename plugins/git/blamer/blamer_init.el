;; -*- lexical-binding: t; -*-

(setq blamer-face '((t :foreground "#7a88cf"
                       :background nil
                       :height 140
                       :italic t)))

(setq blamer-max-lines 1)
;;(setq blamer-type 'echo-area)
;; (setq blamer-show-avatar-p nil)
(setq blamer-idle-time 1.5)

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (require 'blamer)
                (global-blamer-mode 1)
                )))

(global-set-key [(control x) (v) (m)] 'blamer-show-commit-info)

(provide 'blamer_init)

;;; blamer_init.el ends here
