(require 'english-teacher)

;; (require 'eldoc-posframe)
;; (global-eldoc-posframe-mode t)

(setq english-teacher-backend 'tencent)

;; (setq english-teacher-backend 'google)

;; (dolist (hook '(Info-mode-hook
;;                 help-mode-hook
;;                 ))
;;   (add-hook hook '(lambda ()
;;                     (english-teacher-follow-mode 1)
;;                     )))

;; (setq english-teacher-show-result-function 'english-teacher-eldoc-show-result-function)
;; (setq english-teacher-show-result-function 'english-teacher-default-show-result-function)

(provide 'english-teacher_init)

;;; english-teacher_init.el ends here
