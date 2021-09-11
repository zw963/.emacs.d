(require 'zoom-frm)
;; (require 'autofit-frame)
(global-set-key (kbd "C--" ) 'zoom-frm-out)
(global-set-key (kbd "C-=") 'zoom-frm-in)

(global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
                    (vector (list 'control
                                  mouse-wheel-down-event))
                  [C-mouse-wheel])    ; Emacs 20, 21
                'zoom-in)
(when (boundp 'mouse-wheel-up-event) ; Emacs 22+
  (global-set-key (vector (list 'control mouse-wheel-up-event))
                  'zoom-out))

(provide 'zoom-frm_init)
;;; zoom-frm_init.el ends here
