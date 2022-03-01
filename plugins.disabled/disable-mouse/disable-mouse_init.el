(require 'disable-mouse)
(global-disable-mouse-mode)

(defun toggle-disable-mouse ()
  (interactive)
  (if disable-mouse-global-mode
      (global-disable-mouse-mode -1)
    (global-disable-mouse-mode 1)
    ))

(global-set-key [(f11)] 'toggle-disable-mouse)

(provide 'disable-mouse_init)

;;; disable-mouse_init.el ends here
